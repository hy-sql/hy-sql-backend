const { evaluateExpressionPart } = require('./expressionTools')

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
