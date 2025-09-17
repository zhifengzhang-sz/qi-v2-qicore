/**
 * ESLint rule to prevent imperative try/catch blocks in favor of functional patterns
 *
 * Prevents:
 * - try/catch blocks in DSL and actor files
 * - Imperative error handling patterns
 * - Direct error throwing and catching
 *
 * Enforces: Use fromAsyncTryCatch, fromTryCatch from @qi/base
 * All async operations should return Promise<Result<T, QiError>>
 */

import { ESLintUtils, type TSESTree } from "@typescript-eslint/utils";

const createRule = ESLintUtils.RuleCreator(
  (name) => `https://github.com/qi-org/qi-v2-qicore/blob/main/eslint-rules/${name}.md`,
);

export const noImperativeTryCatch = createRule({
  name: "no-imperative-try-catch",
  meta: {
    type: "problem",
    docs: {
      description:
        "Prevent imperative try/catch blocks. Use fromAsyncTryCatch or fromTryCatch from @qi/base instead.",
    },
    messages: {
      noTryCatch: "Avoid imperative try/catch blocks. Use {{suggestion}} from @qi/base instead.",
      suggestAsyncPattern:
        "For async operations, use: fromAsyncTryCatch(async () => { /* operation */ }, error => {{errorType}}Error(error.message))",
      suggestSyncPattern:
        "For sync operations, use: fromTryCatch(() => { /* operation */ }, error => {{errorType}}Error(error.message))",
      preferResultComposition:
        "Consider using Result<T> composition with flatMap() for error handling",
    },
    schema: [
      {
        type: "object",
        properties: {
          enforcePaths: {
            type: "array",
            items: {
              type: "string",
            },
            description:
              'Glob patterns where try/catch is not allowed (default: ["**/lib/dsl/**", "**/lib/actors/**"])',
          },
          allowedFiles: {
            type: "array",
            items: {
              type: "string",
            },
            description: "File patterns where try/catch is still allowed",
          },
          allowTestFiles: {
            type: "boolean",
            description: "Allow try/catch in test files (default: true)",
          },
        },
        additionalProperties: false,
      },
    ],
    hasSuggestions: true,
  },
  defaultOptions: [
    {
      enforcePaths: ["**/lib/dsl/**", "**/lib/actors/**"],
      allowedFiles: [],
      allowTestFiles: true,
    },
  ],
  create(context, [options]) {
    const filename = context.getFilename();

    // Check if current file should enforce functional patterns
    function shouldEnforcePattern(): boolean {
      const enforcePaths = options.enforcePaths || ["**/lib/dsl/**", "**/lib/actors/**"];
      const allowedFiles = options.allowedFiles || [];

      // Skip test files if allowTestFiles is true
      if ((options.allowTestFiles && filename.includes(".test.")) || filename.includes(".spec.")) {
        return false;
      }

      // Skip if file is in allowed list
      if (allowedFiles.some((pattern) => filename.includes(pattern))) {
        return false;
      }

      // Check if file matches enforce path patterns
      return enforcePaths.some((pattern) => {
        const regexPattern = pattern
          .replace(/\*\*/g, ".*")
          .replace(/\*/g, "[^/]*")
          .replace(/\?/g, ".");
        return new RegExp(regexPattern).test(filename);
      });
    }

    // Analyze try block to determine if it's async or sync
    function isTryBlockAsync(tryBlock: TSESTree.BlockStatement): boolean {
      let hasAsyncOperation = false;

      // Simple heuristic: check for await keywords or async function calls
      const visited = new Set();
      const checkNode = (node: any): void => {
        if (!node || visited.has(node)) return;
        visited.add(node);

        if (node.type === "AwaitExpression") {
          hasAsyncOperation = true;
          return;
        }

        if (node.type === "CallExpression" && node.callee) {
          const calleeText = context.getSourceCode().getText(node.callee);
          if (calleeText.includes(".then(") || calleeText.includes(".catch(")) {
            hasAsyncOperation = true;
            return;
          }
        }

        // Recursively check direct child nodes only
        if (node.body && Array.isArray(node.body)) {
          node.body.forEach(checkNode);
        } else if (node.body && typeof node.body === "object") {
          checkNode(node.body);
        }

        if (node.left) checkNode(node.left);
        if (node.right) checkNode(node.right);
        if (node.argument) checkNode(node.argument);
        if (node.expression) checkNode(node.expression);
      };

      tryBlock.body.forEach(checkNode);
      return hasAsyncOperation;
    }

    // Determine appropriate error type from catch parameter or context
    function determineErrorType(catchClause: TSESTree.CatchClause): string {
      if (!catchClause.body) return "system";

      const catchBody = context.getSourceCode().getText(catchClause.body);
      const lowerBody = catchBody.toLowerCase();

      if (lowerBody.includes("network") || lowerBody.includes("connection")) return "network";
      if (lowerBody.includes("valid") || lowerBody.includes("parse")) return "validation";
      if (lowerBody.includes("auth") || lowerBody.includes("permission")) return "authentication";
      if (lowerBody.includes("config") || lowerBody.includes("setting")) return "configuration";
      if (lowerBody.includes("timeout") || lowerBody.includes("deadline")) return "timeout";
      if (lowerBody.includes("business") || lowerBody.includes("rule")) return "business";

      return "system";
    }

    // Generate functional replacement suggestion
    function generateReplacementSuggestion(node: TSESTree.TryStatement): string {
      const isAsync = isTryBlockAsync(node.block);
      const errorType = node.handler ? determineErrorType(node.handler) : "system";
      const tryBlockText = context.getSourceCode().getText(node.block);

      if (isAsync) {
        return `fromAsyncTryCatch(
  async () => ${tryBlockText},
  error => ${errorType}Error(error.message || 'Operation failed')
)`;
      } else {
        return `fromTryCatch(
  () => ${tryBlockText},
  error => ${errorType}Error(error.message || 'Operation failed')
)`;
      }
    }

    if (!shouldEnforcePattern()) {
      return {};
    }

    return {
      TryStatement(node) {
        const isAsync = isTryBlockAsync(node.block);
        const errorType = node.handler ? determineErrorType(node.handler) : "system";
        const suggestion = isAsync ? "fromAsyncTryCatch" : "fromTryCatch";

        context.report({
          node,
          messageId: "noTryCatch",
          data: { suggestion },
          suggest: [
            {
              messageId: isAsync ? "suggestAsyncPattern" : "suggestSyncPattern",
              data: { errorType },
              fix: (fixer) => {
                const replacement = generateReplacementSuggestion(node);
                return fixer.replaceText(node, replacement);
              },
            },
            {
              messageId: "preferResultComposition",
              fix: (fixer) => {
                // This is a conceptual suggestion - no automatic fix
                return [];
              },
            },
          ],
        });
      },

      // Also check for Promise.catch() usage as it's imperative
      CallExpression(node) {
        if (
          node.callee.type === "MemberExpression" &&
          node.callee.property.type === "Identifier" &&
          node.callee.property.name === "catch"
        ) {
          // Check if this is a Promise catch
          const objectText = context.getSourceCode().getText(node.callee.object);
          if (objectText.includes("Promise") || objectText.includes(".then(")) {
            context.report({
              node,
              messageId: "noTryCatch",
              data: { suggestion: "flatMapAsync or Result composition" },
              suggest: [
                {
                  messageId: "preferResultComposition",
                  fix: (fixer) => {
                    // Conceptual suggestion - no automatic fix
                    return [];
                  },
                },
              ],
            });
          }
        }
      },

      // Check for direct Promise constructor with catch
      NewExpression(node) {
        if (node.callee.type === "Identifier" && node.callee.name === "Promise") {
          // Look ahead to see if this Promise is followed by .catch()
          const parent = node.parent;
          if (parent?.type === "MemberExpression" && parent.parent?.type === "CallExpression") {
            const call = parent.parent as TSESTree.CallExpression;
            if (
              call.callee === parent &&
              parent.property.type === "Identifier" &&
              parent.property.name === "catch"
            ) {
              context.report({
                node: call,
                messageId: "noTryCatch",
                data: { suggestion: "fromAsyncTryCatch" },
              });
            }
          }
        }
      },
    };
  },
});

export default noImperativeTryCatch;
