/**
 * ESLint rule to prevent throw statements in DSL files
 *
 * Prevents:
 * - throw statements in any lib/dsl/** files
 * - Ensures DSL layer uses Result<T> pattern exclusively
 * - Enforces functional error handling patterns
 *
 * Rationale: DSL should be pure vocabulary/contracts only
 * All error handling must use Result<T> from @qi/base
 */

import { ESLintUtils } from "@typescript-eslint/utils";

const createRule = ESLintUtils.RuleCreator(
  (name) => `https://github.com/qi-org/qi-v2-qicore/blob/main/eslint-rules/${name}.md`,
);

export const noThrowInDsl = createRule({
  name: "no-throw-in-dsl",
  meta: {
    type: "problem",
    docs: {
      description: "Prevent throw statements in DSL files. DSL must use Result<T> patterns only.",
    },
    messages: {
      noThrowInDsl:
        "DSL files must not contain throw statements. Use Result<T> from @qi/base instead: return failure(error)",
      suggestResultPattern: 'Consider using: return failure({{errorType}}Error("{{message}}"))',
    },
    schema: [
      {
        type: "object",
        properties: {
          dslPaths: {
            type: "array",
            items: {
              type: "string",
            },
            description: 'Glob patterns matching DSL files (default: ["**/lib/dsl/**"])',
          },
          allowedFiles: {
            type: "array",
            items: {
              type: "string",
            },
            description: "File patterns where throws are still allowed",
          },
        },
        additionalProperties: false,
      },
    ],
    hasSuggestions: true,
  },
  defaultOptions: [
    {
      dslPaths: ["**/lib/dsl/**"],
      allowedFiles: [],
    },
  ],
  create(context, [options]) {
    const filename = context.getFilename();
    const sourceCode = context.getSourceCode();

    // Check if current file matches DSL path patterns
    function isDslFile(): boolean {
      const dslPaths = options.dslPaths || ["**/lib/dsl/**"];
      const allowedFiles = options.allowedFiles || [];

      // Skip if file is in allowed list
      if (allowedFiles.some((pattern) => filename.includes(pattern))) {
        return false;
      }

      // Check if file matches DSL path patterns
      return dslPaths.some((pattern) => {
        const regexPattern = pattern
          .replace(/\*\*/g, ".*")
          .replace(/\*/g, "[^/]*")
          .replace(/\?/g, ".");
        return new RegExp(regexPattern).test(filename);
      });
    }

    // Extract error message from throw statement for suggestions
    function extractErrorMessage(node: any): string {
      if (
        node.argument?.type === "NewExpression" &&
        node.argument.callee?.name === "Error" &&
        node.argument.arguments?.[0]?.type === "Literal"
      ) {
        return node.argument.arguments[0].value as string;
      }
      return "Operation failed";
    }

    // Determine appropriate error type based on context
    function suggestErrorType(message: string): string {
      const msg = message.toLowerCase();
      if (msg.includes("network") || msg.includes("connection")) return "network";
      if (msg.includes("validate") || msg.includes("invalid")) return "validation";
      if (msg.includes("auth") || msg.includes("permission")) return "authentication";
      if (msg.includes("config") || msg.includes("setting")) return "configuration";
      if (msg.includes("timeout") || msg.includes("deadline")) return "timeout";
      return "system";
    }

    if (!isDslFile()) {
      return {};
    }

    return {
      ThrowStatement(node) {
        const errorMessage = extractErrorMessage(node);
        const errorType = suggestErrorType(errorMessage);

        context.report({
          node,
          messageId: "noThrowInDsl",
          suggest: [
            {
              messageId: "suggestResultPattern",
              data: {
                errorType,
                message: errorMessage,
              },
              fix: (fixer) => {
                // Generate Result<T> pattern replacement
                const replacement = `return failure(${errorType}Error("${errorMessage}"))`;
                return fixer.replaceText(node, replacement);
              },
            },
          ],
        });
      },

      // Also catch Error constructor calls that might be thrown later
      NewExpression(node) {
        if (node.callee.type === "Identifier" && node.callee.name === "Error") {
          // Check if this Error is being assigned to a variable that gets thrown
          const parent = node.parent;
          if (parent?.type === "VariableDeclarator") {
            const varName = parent.id.type === "Identifier" ? parent.id.name : null;
            if (varName) {
              // Look for throw statements using this variable
              const scope = context.getScope();
              const refs = scope.references.filter((ref) => ref.identifier.name === varName);

              for (const ref of refs) {
                if (ref.identifier.parent?.type === "ThrowStatement") {
                  context.report({
                    node: ref.identifier.parent,
                    messageId: "noThrowInDsl",
                  });
                }
              }
            }
          }
        }
      },
    };
  },
});

export default noThrowInDsl;
