const _ = require('lodash')
const {
    containsFunctionPattern,
    functionExpressionPattern,
} = require('../../helpers/regex')

const splitBracketsFromFunctionExpressionArray = (
    functionConditionSurroundedWithBrackets
) => {
    const arr = functionConditionSurroundedWithBrackets
        .replace(functionExpressionPattern, ' $1 ')
        .split(' ')

    const newArr = arr.map((e) => {
        if (!containsFunctionPattern.test(e)) {
            return e.split(/(\()|(\))/).filter(Boolean)
        }
        return e
    })

    return _.flatten(newArr)
}

module.exports = splitBracketsFromFunctionExpressionArray
