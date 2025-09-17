/**
 * ESLint rule to enforce readonly properties in DSL interfaces
 *
 * Prevents:
 * - Mutable properties in DSL interface definitions
 * - Non-readonly array and object types
 * - Mutable class properties in DSL files
 *
 * Enforces: All DSL types must be immutable contracts
 * DSL represents pure vocabulary - should never be modified
 */

import { ESLintUtils, type TSESTree } from "@typescript-eslint/utils";

const createRule = ESLintUtils.RuleCreator(
  (name) => `https://github.com/qi-org/qi-v2-qicore/blob/main/eslint-rules/${name}.md`,
);

export const enforceReadonlyDsl = createRule({
  name: "enforce-readonly-dsl",
  meta: {
    type: "problem",
    docs: {
      description: "Enforce readonly properties in DSL interface and type definitions",
    },
    messages: {
      mustBeReadonly: 'DSL {{nodeType}} properties must be readonly. Add "readonly" modifier.',
      arrayMustBeReadonly: 'DSL arrays must use "readonly T[]" instead of "T[]"',
      objectMustBeReadonly: "DSL object types should use readonly properties",
      classNotAllowed:
        "DSL files should not contain class definitions. Use interfaces or types instead.",
      methodNotAllowed:
        "DSL interfaces should not contain methods. Use function type properties instead: readonly methodName: (params) => Result<T>",
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
            description: "File patterns where mutable properties are allowed",
          },
          strictArrays: {
            type: "boolean",
            description: "Enforce readonly arrays (default: true)",
          },
        },
        additionalProperties: false,
      },
    ],
    fixable: "code",
  },
  defaultOptions: [
    {
      dslPaths: ["**/lib/dsl/**"],
      allowedFiles: [],
      strictArrays: true,
    },
  ],
  create(context, [options]) {
    const filename = context.getFilename();

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

    // Check if type annotation is a readonly array
    function isReadonlyArray(typeAnnotation: TSESTree.TypeNode): boolean {
      if (
        typeAnnotation.type === "TSTypeOperator" &&
        typeAnnotation.operator === "readonly" &&
        typeAnnotation.typeAnnotation &&
        typeAnnotation.typeAnnotation.type === "TSArrayType"
      ) {
        return true;
      }

      if (
        typeAnnotation.type === "TSTypeReference" &&
        typeAnnotation.typeName.type === "Identifier" &&
        typeAnnotation.typeName.name === "ReadonlyArray"
      ) {
        return true;
      }

      return false;
    }

    // Check if type annotation contains mutable arrays
    function hasMutableArray(typeAnnotation: TSESTree.TypeNode): boolean {
      if (typeAnnotation.type === "TSArrayType") {
        return true; // T[] is mutable
      }

      if (typeAnnotation.type === "TSUnionType" || typeAnnotation.type === "TSIntersectionType") {
        return typeAnnotation.types.some(hasMutableArray);
      }

      return false;
    }

    if (!isDslFile()) {
      return {};
    }

    return {
      // Check interface property signatures
      TSPropertySignature(node) {
        if (!node.readonly) {
          const nodeType = node.parent?.type === "TSInterfaceDeclaration" ? "interface" : "type";

          context.report({
            node,
            messageId: "mustBeReadonly",
            data: { nodeType },
            fix: (fixer) => {
              return fixer.insertTextBefore(node, "readonly ");
            },
          });
        }

        // Check for mutable arrays if strictArrays is enabled
        if (options.strictArrays && node.typeAnnotation?.typeAnnotation) {
          const typeAnnotation = node.typeAnnotation.typeAnnotation;

          if (typeAnnotation && hasMutableArray(typeAnnotation)) {
            context.report({
              node: typeAnnotation,
              messageId: "arrayMustBeReadonly",
              fix: (fixer) => {
                if (typeAnnotation.type === "TSArrayType") {
                  const elementType = context.getSourceCode().getText(typeAnnotation.elementType);
                  return fixer.replaceText(typeAnnotation, `readonly ${elementType}[]`);
                }
                return null;
              },
            });
          }
        }
      },

      // Note: TSPropertySignature handler above already covers type literals

      // Check method signatures (should be function type properties instead)
      TSMethodSignature(node) {
        context.report({
          node,
          messageId: "methodNotAllowed",
          fix: (fixer) => {
            const sourceCode = context.getSourceCode();
            const key = sourceCode.getText(node.key);
            const params = node.params.map((param) => sourceCode.getText(param)).join(", ");

            // Determine return type - should be Result<T> for DSL
            let returnType = "Result<unknown, QiError>";
            if (node.returnType) {
              const returnTypeText = sourceCode.getText(node.returnType.typeAnnotation);
              if (!returnTypeText.includes("Result<")) {
                returnType = `Result<${returnTypeText}, QiError>`;
              } else {
                returnType = returnTypeText;
              }
            }

            const replacement = `readonly ${key}: (${params}) => ${returnType}`;
            return fixer.replaceText(node, replacement);
          },
        });
      },

      // Disallow class definitions in DSL
      ClassDeclaration(node) {
        context.report({
          node,
          messageId: "classNotAllowed",
        });
      },

      // Check property definitions (in case classes slip through)
      PropertyDefinition(node) {
        if (!node.readonly) {
          context.report({
            node,
            messageId: "mustBeReadonly",
            data: { nodeType: "class" },
            fix: (fixer) => {
              return fixer.insertTextBefore(node, "readonly ");
            },
          });
        }
      },

      // Check for mutable arrays in type aliases
      TSTypeAliasDeclaration(node) {
        if (options.strictArrays && hasMutableArray(node.typeAnnotation)) {
          context.report({
            node: node.typeAnnotation,
            messageId: "arrayMustBeReadonly",
          });
        }
      },

      // Check tuple types for readonly
      TSTupleType(node) {
        if (options.strictArrays && node.parent?.type !== "TSTypeOperator") {
          // Check if parent is readonly operator
          const parent = node.parent;
          const grandparent = parent?.parent;

          if (!(grandparent?.type === "TSTypeOperator" && grandparent.operator === "readonly")) {
            context.report({
              node,
              messageId: "arrayMustBeReadonly",
              fix: (fixer) => {
                return fixer.insertTextBefore(node, "readonly ");
              },
            });
          }
        }
      },
    };
  },
});

export default enforceReadonlyDsl;
