const _ = require('lodash')
const splitBracketsFromFunctionExpressionArray = require('./splitBracketsFromFunctionExpressionArray')
const { containsFunctionPattern } = require('../../helpers/regex')
const {
    transformCommandArrayIntoConditionsArray,
} = require('./arrayTransformationTools')

const prepareConditionsForParsing = (slicedCommandArray) => {
    const conditionsArray = transformCommandArrayIntoConditionsArray(
        slicedCommandArray
    )

    return splitBracketsFromConditionsArray(conditionsArray)
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
