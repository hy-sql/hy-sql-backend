const {
    executeStringFunction,
    executeAggregateFunction,
} = require('./functions')
const { calculateExpression } = require('../../utils/calculateExpression')
const SQLError = require('../../models/SQLError')

/**
 * Takes an array of expression fields and maps them using evaluateExpressionPart(expressionPart, row),
 * using columns in the expression with given row values. From The results creates
 * a string type of expression and calculates using calculateExpression(mappedExpression)
 * @param {Array} expressionArray
 * @param {Object} row
 *
 * const expressionArray = [
 *     { type: 'integer', value: 5 },
 *     { type: 'operator', value: '+' },
 *     { type: 'column', value: 'price' },
 *     { type: 'operator', value: '*' },
 *     { type: 'integer', value: 4 }
 *   ]
 *
 * const row = { id: 6, name: 'celery', price: 6, amount: 70 }
 *
 * mappedExpression is '5+6*4'
 * output is 29
 */
const evaluateExpression = (expressionArray, row) => {
    const mappedExpression = expressionArray
        .map((e) => evaluateExpressionPart(e, row))
        .join('')

    return calculateExpression(mappedExpression)
}

/**
 * Returns result of a single expression part
 * @param {Object} expressionPart
 * @param {Object} row
 */
const evaluateExpressionPart = (expressionPart, row) => {
    switch (expressionPart.type) {
        case 'expression': {
            const expressionResult = evaluateExpression(
                expressionPart.expressionParts,
                row
            )
            return expressionResult
        }
        case 'stringFunction':
            return executeStringFunction(expressionPart, row)
        case 'aggregateFunction':
            return executeAggregateFunction(expressionPart, row)
        case 'text':
            return expressionPart.value
        case 'integer':
            return expressionPart.value
        case 'column':
            if (!row[expressionPart.value] && row[expressionPart.value] !== 0) {
                throw new SQLError(`No such column: ${expressionPart.value}`)
            }
            return row[expressionPart.value]
        case 'operator':
            return expressionPart.value
    }
}

/**
 * Same as evaluateExpression above, only takes aggregate expression and an array of rows as an input
 * @param {Array} expressionArray
 * @param {Array} rows
 */
const evaluateAggregateExpression = (expressionArray, rows) => {
    const evaluatedExpression = expressionArray.map((e) =>
        evaluateAggregateExpressionPart(e, rows)
    )

    return calculateExpression(evaluatedExpression)
}

/**
 * Returns result of a single aggregate expression part
 * @param {Object} expressionPart
 * @param {Array} rows
 */
const evaluateAggregateExpressionPart = (expressionPart, rows) => {
    switch (expressionPart.type) {
        case 'stringFunction':
            return executeStringFunction(expressionPart, rows[0])
        case 'aggregateFunction':
            return executeAggregateFunction(expressionPart, rows)
        case 'column':
            if (
                !rows[0][expressionPart.value] &&
                rows[0][expressionPart.value] !== 0
            ) {
                throw new SQLError(`No such column: ${expressionPart.value}`)
            }
            return rows[0].expressionPart.value
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
