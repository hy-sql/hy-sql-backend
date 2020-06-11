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

/*Creates filter that gives the opposite results than filter created with createFilter()*/
const createOppositeFilter = (whereObject) => {
    const column = whereObject.columnName
    const value = whereObject.value
    const sign = whereObject.sign

    switch (sign) {
        case '<>':
            return (item) => {
                // eslint-disable-next-line
                return item[column] == value
            }
        case '>':
            return (item) => {
                return item[column] <= value
            }
        case '<':
            return (item) => {
                return item[column] >= value
            }
        case '>=':
            return (item) => {
                return item[column] < value
            }
        case '<=':
            return (item) => {
                return item[column] > value
            }
        default:
            return (item) => {
                // eslint-disable-next-line
                return item[column] != value
            }
    }
}

module.exports = {
    createFilter,
    createOppositeFilter,
}
