const _ = require('lodash')
const parseField = require('./fieldParser')
const {
    arithmeticOperatorPattern,
    containsFunctionPattern,
} = require('../../helpers/regex')

const parseExpression = (expression) => {
    const splitExpression = _.flatten(
        expression
            .replace(containsFunctionPattern, ' $1 ')
            .split(' ')
            .filter(Boolean)
            .map((e) =>
                containsFunctionPattern.test(e)
                    ? e
                    : e.split(arithmeticOperatorPattern).filter(Boolean)
            )
            .filter(Boolean)
    )

    // return splitExpression.map((e) => parseExpressionFields(e))
    return splitExpression.map((e) => parseField(e))
}

module.exports = parseExpression
