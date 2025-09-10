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

import { ESLintUtils, TSESTree } from '@typescript-eslint/utils'

const createRule = ESLintUtils.RuleCreator(
  name => `https://github.com/qi-org/qi-v2-qicore/blob/main/eslint-rules/${name}.md`,
)

export const noResultAntiPatterns = createRule({
  name: 'no-result-anti-patterns',
  meta: {
    type: 'problem',
    docs: {
      description: 'Prevent anti-patterns when using Result<T> from @qi/base',
    },
    messages: {
      directTagCheck: 'Do not check result.tag directly. Use match(), isSuccess(), or isFailure() instead.',
      directPropertyAccess: 'Do not access result.{{property}} directly. Use match(), map(), or flatMap() instead.',
      resultDestructuring: 'Do not destructure Result objects. Use match(), map(), or flatMap() instead.',
    },
    schema: [],
  },
  defaultOptions: [],
  create(context) {
    const sourceCode = context.getSourceCode()
    
    // Check if a node represents a Result type based on TypeScript info
    function isResultType(node: TSESTree.Node): boolean {
      const services = ESLintUtils.getParserServices(context)
      const checker = services.program.getTypeChecker()
      const tsNode = services.esTreeNodeToTSNodeMap.get(node)
      const type = checker.getTypeAtLocation(tsNode)
      const typeString = checker.typeToString(type)
      
      // Check if it's a Result type (basic heuristic)
      return typeString.includes('Result<') || 
             typeString.includes('{ tag: "success"') || 
             typeString.includes('{ tag: "failure"')
    }

    return {
      // Prevent: result.tag === 'success'/'failure'
      BinaryExpression(node) {
        if (
          node.operator === '===' || node.operator === '==' ||
          node.operator === '!==' || node.operator === '!='
        ) {
          // Check if left side is result.tag
          if (
            node.left.type === 'MemberExpression' &&
            node.left.property.type === 'Identifier' &&
            node.left.property.name === 'tag'
          ) {
            // Check if right side is 'success' or 'failure'
            if (
              node.right.type === 'Literal' &&
              (node.right.value === 'success' || node.right.value === 'failure')
            ) {
              if (isResultType(node.left.object)) {
                context.report({
                  node,
                  messageId: 'directTagCheck',
                })
              }
            }
          }
          
          // Also check flipped: 'success' === result.tag
          if (
            node.right.type === 'MemberExpression' &&
            node.right.property.type === 'Identifier' &&
            node.right.property.name === 'tag' &&
            node.left.type === 'Literal' &&
            (node.left.value === 'success' || node.left.value === 'failure')
          ) {
            if (isResultType(node.right.object)) {
              context.report({
                node,
                messageId: 'directTagCheck',
              })
            }
          }
        }
      },

      // Prevent: result.value, result.error (but allow result.tag for error messages)
      MemberExpression(node) {
        if (
          node.property.type === 'Identifier' &&
          (node.property.name === 'value' || node.property.name === 'error')
        ) {
          if (isResultType(node.object)) {
            context.report({
              node,
              messageId: 'directPropertyAccess',
              data: {
                property: node.property.name,
              },
            })
          }
        }
      },

      // Prevent: const { tag, value, error } = result
      VariableDeclarator(node) {
        if (
          node.id.type === 'ObjectPattern' &&
          node.init &&
          isResultType(node.init)
        ) {
          // Check if destructuring includes Result-specific properties
          const hasResultProps = node.id.properties.some(prop => {
            if (prop.type === 'Property' && prop.key.type === 'Identifier') {
              return ['tag', 'value', 'error'].includes(prop.key.name)
            }
            return false
          })
          
          if (hasResultProps) {
            context.report({
              node,
              messageId: 'resultDestructuring',
            })
          }
        }
      },

      // Prevent: const { tag, value } = someResult in assignments
      AssignmentExpression(node) {
        if (
          node.left.type === 'ObjectPattern' &&
          node.right &&
          isResultType(node.right)
        ) {
          const hasResultProps = node.left.properties.some(prop => {
            if (prop.type === 'Property' && prop.key.type === 'Identifier') {
              return ['tag', 'value', 'error'].includes(prop.key.name)
            }
            return false
          })
          
          if (hasResultProps) {
            context.report({
              node,
              messageId: 'resultDestructuring',
            })
          }
        }
      },
    }
  },
})

export default noResultAntiPatterns