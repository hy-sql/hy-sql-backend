const {
    executeStringFunction,
    executeAggregateFunction,
} = require('./functions')
const { calculateExpression } = require('../../utils/calculateExpression')

const evaluateExpression = (expressionArray, row, rows) => {
    const mappedExpression = expressionArray
        .map((e) => evaluateExpressionPart(e, row, rows))
        .join('')

    return calculateExpression(mappedExpression)
}

const evaluateExpressionPart = (expressionPart, row) => {
    switch (expressionPart.type) {
        case 'expression': {
            const expressionResult = evaluateExpression(
                expressionPart.value,
                row
            )
            return expressionResult
        }
        case 'stringFunction':
            return executeStringFunction(expressionPart, row)
        case 'aggregateFunction':
            return executeAggregateFunction(expressionPart)
        case 'string':
            return expressionPart.value
        case 'integer':
            return expressionPart.value
        case 'column':
            return row[expressionPart.value]
        case 'operator':
            return expressionPart.value
    }
}

const evaluateAggregateExpression = (expressionArray, rows) => {
    const evaluatedExpression = expressionArray.map((e) =>
        evaluateAggregateExpressionPart(e, rows)
    )

    return calculateExpression(evaluatedExpression)
}

const evaluateAggregateExpressionPart = (expressionPart, rows) => {
    switch (expressionPart.type) {
        case 'stringFunction':
            return executeStringFunction(expressionPart, rows[0])
        case 'aggregateFunction':
            return executeAggregateFunction(expressionPart, rows)
        case 'column':
            return rows[0].column
        default:
            return expressionPart.value
    }
}

const containsAggregateFunction = (expression) => {
    return expression.some((e) => e.type === 'aggregateFunction')
}

module.exports = {
    evaluateExpression,
    evaluateExpressionPart,
    evaluateAggregateExpression,
    evaluateAggregateExpressionPart,
    containsAggregateFunction,
}
