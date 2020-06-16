const _ = require('lodash')
const splitBracketsFromFunctionExpressionArray = require('./splitBracketsFromFunctionExpressionArray')
const {
    containsFunctionPattern,
    comparisonOperatorPatternWithWhiteSpace,
    containsFunctionPatternWithWhiteSpaces,
} = require('../../helpers/regex')

const prepareConditionsForParsing = (slicedCommandArray) => {
    const conditionsArray = transformCommandArrayIntoConditionsArray(
        slicedCommandArray
    )

    return splitBracketsFromConditionsArray(conditionsArray)
}

const transformCommandArrayIntoConditionsArray = (commandArray) => {
    const conditionsArray = commandArray
        .join(' ')
        .replace(containsFunctionPatternWithWhiteSpaces, (m) =>
            m.replace(/\s+/g, '')
        )
        .replace(comparisonOperatorPatternWithWhiteSpace, (m) =>
            m.replace(/\s+/g, '')
        )
        .replace(/AND/gi, ' AND ')
        .replace(/OR/gi, ' OR ')
        .split(/ +(?=(?:(?:[^']*'){2})*[^']*$)/g)
        .filter(Boolean)

    return conditionsArray
}

const splitBracketsFromConditionsArray = (conditionsArray) => {
    const splitConditionsArray = conditionsArray.reduce((newArray, c) => {
        if (!c.match(containsFunctionPattern)) {
            newArray.push(
                c
                    .replace('(', ' ( ')
                    .replace(')', ' ) ')
                    .split(/ +(?=(?:(?:[^']*'){2})*[^']*$)/g)
            )
        } else {
            const splitArray = splitBracketsFromFunctionExpressionArray(c)
            splitArray.forEach((e) => newArray.push(e))
        }

        return newArray
    }, [])

    return _.flatten(splitConditionsArray).filter(Boolean)
}

module.exports = prepareConditionsForParsing
