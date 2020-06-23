const _ = require('lodash')
const {
    containsArithmeticOperatorPattern,
    containsFunctionPattern,
} = require('../../helpers/regex')
const { containsFunction } = require('../../helpers/containsRegexTools')

/**
 * Transforms arithmetic expression from String into Array of individual expression parts
 * @param {String} expression
 *
 * const input = 'price*LENGTH(name)+5'
 * const output = splitExpressionIntoArray(input)
 * output is [ 'price', '*', 'LENGTH(name)', '+', '5' ]
 */
const splitExpressionIntoArray = (expression) => {
    const splitExpression = _.flatten(
        expression
            .replace(containsFunctionPattern, ' $1 ')
            .split(' ')
            .filter(Boolean)
            .map((e) =>
                containsFunction(e)
                    ? e
                    : e.split(containsArithmeticOperatorPattern).filter(Boolean)
            )
            .filter(Boolean)
    )

    return splitExpression
}

module.exports = splitExpressionIntoArray
