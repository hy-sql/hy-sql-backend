const { evaluateExpressionPart } = require('./expressionTools')

/**
 * Creates a filter for lodash _.filter for a row based on the given condition.
 * @param {object} row the row in question
 * @param {object} condition the condition
 */
const createFilter = (row, condition) => {
    switch (condition.operator) {
        case '=':
            return (
                // eslint-disable-next-line
                evaluateExpressionPart(condition.left, row) ==
                evaluateExpressionPart(condition.right, row)
            )
        case '>':
            return (
                evaluateExpressionPart(condition.left, row) >
                evaluateExpressionPart(condition.right, row)
            )
        case '<':
            return (
                evaluateExpressionPart(condition.left, row) <
                evaluateExpressionPart(condition.right, row)
            )
        case '>=':
            return (
                evaluateExpressionPart(condition.left, row) >=
                evaluateExpressionPart(condition.right, row)
            )
        case '<=':
            return (
                evaluateExpressionPart(condition.left, row) <=
                evaluateExpressionPart(condition.right, row)
            )
        case '<>':
            return (
                // eslint-disable-next-line
                evaluateExpressionPart(condition.left, row) !=
                evaluateExpressionPart(condition.right, row)
            )
    }
}

module.exports = { createFilter }
