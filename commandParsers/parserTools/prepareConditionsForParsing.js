const _ = require('lodash')
const splitBracketsFromFunctionExpressionArray = require('./splitBracketsFromFunctionExpressionArray')
const { containsFunctionPattern } = require('../../helpers/regex')

const prepareConditionsForParsing = (slicedCommandArray) => {
    const conditionsArray = slicedCommandArray
        .join('')
        .replace(/AND/gi, ' AND ')
        .replace(/OR/gi, ' OR ')
        .split(' ')
        .filter(Boolean)

    const newArray = conditionsArray.reduce((newArray, c) => {
        if (!c.match(containsFunctionPattern)) {
            newArray.push(c.replace('(', ' ( ').replace(')', ' ) ').split(' '))
        } else {
            const splitArray = splitBracketsFromFunctionExpressionArray(c)
            splitArray.forEach((e) => newArray.push(e))
        }

        return newArray
    }, [])

    return _.flatten(newArray).filter(Boolean)
}

module.exports = prepareConditionsForParsing
