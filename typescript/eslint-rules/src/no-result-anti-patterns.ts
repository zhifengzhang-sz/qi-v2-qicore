/**
 * ESLint rule to prevent Result<T> anti-patterns in @qi/base
 *
 * Prevents:
 * - Direct tag checking: result.tag === 'success'
 * - Direct property access: result.value, result.error
 * - Destructuring: const { tag, value } = result
 *
 * Enforces using: match(), map(), flatMap(), isSuccess(), isFailure()
 */

import { ESLintUtils } from "@typescript-eslint/utils";

const createRule = ESLintUtils.RuleCreator(
  (name) => `https://github.com/qi-org/qi-v2-qicore/blob/main/eslint-rules/${name}.md`,
);

export interface RuleOptions {
  excludePatterns?: string[];
}

export const noResultAntiPatterns = createRule<[RuleOptions], string>({
  name: "no-result-anti-patterns",
  meta: {
    type: "problem",
    docs: {
      description: "Prevent anti-patterns when using Result<T> from @qi/base",
    },
    messages: {
      directTagCheck:
        "Do not check result.tag directly. Use match(), isSuccess(), or isFailure() instead.",
      directPropertyAccess:
        "Do not access result.{{property}} directly. Use match(), map(), or flatMap() instead.",
      resultDestructuring:
        "Do not destructure Result objects. Use match(), map(), or flatMap() instead.",
      switchOnTag:
        "Do not switch on result.tag. Use match() for pattern matching instead.",
      ternaryOnTag:
        "Do not use ternary operators with result.tag. Use match() or isSuccess()/isFailure() instead.",
      logicalWithTag:
        "Do not use logical operators with result.tag. Use match() or isSuccess()/isFailure() instead.",
    },
    schema: [
      {
        type: "object",
        properties: {
          excludePatterns: {
            type: "array",
            items: {
              type: "string"
            },
            description: "Array of path patterns to exclude from anti-pattern checking"
          }
        },
        additionalProperties: false
      }
    ],
  },
  defaultOptions: [{ excludePatterns: [] }],
  create(context) {
    const sourceCode = context.getSourceCode();
    const filename = context.getFilename ? context.getFilename() : context.filename;
    const options = context.options[0] || {};
    const excludePatterns = options.excludePatterns || [];

    // Check if file should be excluded based on configuration
    const normalizedPath = filename.replace(/\\/g, '/');
    const shouldExclude = excludePatterns.some((pattern: string) =>
      normalizedPath.includes(pattern)
    );

    if (shouldExclude) {
      return {};
    }

    // Check if a node represents a Result type based on TypeScript type information
    function isResultType(node: any): boolean {
      try {
        const services = ESLintUtils.getParserServices(context);
        const checker = services.program.getTypeChecker();
        const tsNode = services.esTreeNodeToTSNodeMap.get(node);

        if (!tsNode) return false;

        const type = checker.getTypeAtLocation(tsNode);
        const typeString = checker.typeToString(type);

        // Primary check: explicit Result types from @qi/base
        if (
          typeString.includes("Result<") ||
          typeString.includes("Success<") ||
          typeString.includes("Failure<") ||
          (type.symbol && type.symbol.name === "Result")
        ) {
          return true;
        }

        // Secondary check: discriminated union structure matching Result<T>
        if (
          (typeString.includes('{ tag: "success"') && typeString.includes("value:")) ||
          (typeString.includes('{ tag: "failure"') && typeString.includes("error:"))
        ) {
          return true;
        }

        // Enhanced check: union types that include success/failure tags
        if (type.isUnion && type.isUnion()) {
          return type.types.some(unionType => {
            const unionTypeString = checker.typeToString(unionType);
            return (
              (unionTypeString.includes('tag: "success"') && unionTypeString.includes("value")) ||
              (unionTypeString.includes('tag: "failure"') && unionTypeString.includes("error"))
            );
          });
        }

        // Check for object types with tag property of literal type 'success' | 'failure'
        const tagSymbol = type.getProperty?.("tag");
        if (tagSymbol) {
          const tagType = checker.getTypeOfSymbolAtLocation(tagSymbol, tsNode);
          const tagTypeString = checker.typeToString(tagType);
          if (tagTypeString.includes('"success"') || tagTypeString.includes('"failure"')) {
            // Also check for value or error properties to confirm it's Result-like
            const hasValue = type.getProperty?.("value");
            const hasError = type.getProperty?.("error");
            if (hasValue || hasError) {
              return true;
            }
          }
        }

        return false;
      } catch (error) {
        // Improved fallback with more conservative heuristic
        const text = sourceCode.getText(node);

        // Only match if it's clearly a result variable (not just any text containing "result")
        if (node.type === "Identifier") {
          return /^(result|res|outcome|op|operation)$/i.test(node.name);
        }

        // For member expressions, check the object name
        if (node.type === "MemberExpression" && node.object?.type === "Identifier") {
          return /^(result|res|outcome|op|operation)$/i.test(node.object.name);
        }

        // More conservative fallback
        return text.match(/\b(result|res)\b.*\.tag/) !== null;
      }
    }

    return {
      // Prevent: switch (result.tag) { case 'success': ... }
      SwitchStatement(node) {
        if (
          node.discriminant.type === "MemberExpression" &&
          node.discriminant.property.type === "Identifier" &&
          node.discriminant.property.name === "tag"
        ) {
          // Check if any case has success/failure literals
          const hasResultCases = node.cases.some((caseNode) => {
            if (caseNode.test && caseNode.test.type === "Literal") {
              return caseNode.test.value === "success" || caseNode.test.value === "failure";
            }
            return false;
          });

          if (hasResultCases && isResultType(node.discriminant.object)) {
            context.report({
              node,
              messageId: "switchOnTag",
            });
          }
        }
      },

      // Prevent: result.tag === 'success' ? ... : ...
      ConditionalExpression(node) {
        if (
          node.test.type === "BinaryExpression" &&
          (node.test.operator === "===" || node.test.operator === "==" ||
           node.test.operator === "!==" || node.test.operator === "!=")
        ) {
          // Check if the test is result.tag comparison
          if (
            node.test.left.type === "MemberExpression" &&
            node.test.left.property.type === "Identifier" &&
            node.test.left.property.name === "tag" &&
            node.test.right.type === "Literal" &&
            (node.test.right.value === "success" || node.test.right.value === "failure")
          ) {
            if (isResultType(node.test.left.object)) {
              context.report({
                node: node.test,
                messageId: "ternaryOnTag",
              });
            }
          }
          // Also check flipped: 'success' === result.tag ? ... : ...
          if (
            node.test.right.type === "MemberExpression" &&
            node.test.right.property.type === "Identifier" &&
            node.test.right.property.name === "tag" &&
            node.test.left.type === "Literal" &&
            (node.test.left.value === "success" || node.test.left.value === "failure")
          ) {
            if (isResultType(node.test.right.object)) {
              context.report({
                node: node.test,
                messageId: "ternaryOnTag",
              });
            }
          }
        }
      },

      // Prevent: result.tag === 'success' && ..., result.tag === 'failure' || ...
      LogicalExpression(node) {
        function checkForTagComparison(expr: any): boolean {
          if (expr.type === "BinaryExpression") {
            // Check left side: result.tag === 'success'
            if (
              expr.left.type === "MemberExpression" &&
              expr.left.property.type === "Identifier" &&
              expr.left.property.name === "tag" &&
              expr.right.type === "Literal" &&
              (expr.right.value === "success" || expr.right.value === "failure")
            ) {
              return isResultType(expr.left.object);
            }
            // Check flipped: 'success' === result.tag
            if (
              expr.right.type === "MemberExpression" &&
              expr.right.property.type === "Identifier" &&
              expr.right.property.name === "tag" &&
              expr.left.type === "Literal" &&
              (expr.left.value === "success" || expr.left.value === "failure")
            ) {
              return isResultType(expr.right.object);
            }
          }
          return false;
        }

        if (checkForTagComparison(node.left)) {
          context.report({
            node: node.left,
            messageId: "logicalWithTag",
          });
        }
        if (checkForTagComparison(node.right)) {
          context.report({
            node: node.right,
            messageId: "logicalWithTag",
          });
        }
      },

      // Prevent: result.tag === 'success'/'failure'
      BinaryExpression(node) {
        if (
          node.operator === "===" ||
          node.operator === "==" ||
          node.operator === "!==" ||
          node.operator === "!="
        ) {
          // Check if left side is result.tag
          if (
            node.left.type === "MemberExpression" &&
            node.left.property.type === "Identifier" &&
            node.left.property.name === "tag"
          ) {
            // Check if right side is 'success' or 'failure'
            if (
              node.right.type === "Literal" &&
              (node.right.value === "success" || node.right.value === "failure")
            ) {
              if (isResultType(node.left.object)) {
                context.report({
                  node,
                  messageId: "directTagCheck",
                });
              }
            }
          }

          // Also check flipped: 'success' === result.tag
          if (
            node.right.type === "MemberExpression" &&
            node.right.property.type === "Identifier" &&
            node.right.property.name === "tag" &&
            node.left.type === "Literal" &&
            (node.left.value === "success" || node.left.value === "failure")
          ) {
            if (isResultType(node.right.object)) {
              context.report({
                node,
                messageId: "directTagCheck",
              });
            }
          }
        }
      },

      // Prevent: result.value, result.error (but allow result.tag for error messages)
      MemberExpression(node) {
        if (
          node.property.type === "Identifier" &&
          (node.property.name === "value" || node.property.name === "error")
        ) {
          if (isResultType(node.object)) {
            context.report({
              node,
              messageId: "directPropertyAccess",
              data: {
                property: node.property.name,
              },
            });
          }
        }
      },

      // Prevent: const { tag, value, error } = result
      VariableDeclarator(node) {
        if (node.id.type === "ObjectPattern" && node.init && isResultType(node.init)) {
          // Check if destructuring includes Result-specific properties
          const hasResultProps = node.id.properties.some((prop: any) => {
            if (prop.type === "Property" && prop.key.type === "Identifier") {
              return ["tag", "value", "error"].includes(prop.key.name);
            }
            return false;
          });

          if (hasResultProps) {
            context.report({
              node,
              messageId: "resultDestructuring",
            });
          }
        }
      },

      // Prevent: const { tag, value } = someResult in assignments
      AssignmentExpression(node) {
        if (node.left.type === "ObjectPattern" && node.right && isResultType(node.right)) {
          const hasResultProps = node.left.properties.some((prop: any) => {
            if (prop.type === "Property" && prop.key.type === "Identifier") {
              return ["tag", "value", "error"].includes(prop.key.name);
            }
            return false;
          });

          if (hasResultProps) {
            context.report({
              node,
              messageId: "resultDestructuring",
            });
          }
        }
      },
    };
  },
});

export default noResultAntiPatterns;
