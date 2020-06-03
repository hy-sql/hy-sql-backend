const Formula = require('@hapi/formula')

/**
 *
 * @param {string} expression
 * @param {object} context - optional object with runtime formula context used to resolve variables
 *                           eg. if yor expression is '4*column-2' the object must be { column: yourValueHere }
 *                           details: https://hapi.dev/module/formula/
 * @returns {Number}
 */
const calculateExpression = (expression, context) => {
    const formula = new Formula.Parser(expression)

    return formula.evaluate(context)
}

module.exports = { calculateExpression }
