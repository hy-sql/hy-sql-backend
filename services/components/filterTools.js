const { evaluateExpressionPart } = require('./expressionTools')

const createAdvancedFilter = (row, condition) => {
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

const createFilter = (whereObject) => {
    const column = whereObject.columnName
    const value = whereObject.value
    const sign = whereObject.sign

    switch (sign) {
        case '>':
            return (item) => {
                return item[column] > value
            }
        case '<':
            return (item) => {
                return item[column] < value
            }
        case '>=':
            return (item) => {
                return item[column] >= value
            }
        case '<=':
            return (item) => {
                return item[column] <= value
            }
        case '<>':
            return (item) => {
                // eslint-disable-next-line eqeqeq
                return item[column] != value
            }
        default:
            return (item) => {
                // eslint-disable-next-line eqeqeq
                return item[column] == value
            }
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
    createAdvancedFilter,
    createFilter,
    createOppositeFilter,
}
