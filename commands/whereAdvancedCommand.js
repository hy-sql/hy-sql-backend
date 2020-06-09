const _ = require('lodash')
const {
    arithmeticOperatorPattern,
    arithmeticExpressionPattern,
    comparisonOperatorPattern,
    stringFunctionPattern,
    containsFunctionPattern,
    functionExpressionPattern,
} = require('../utils/regex')
const {
    parseParameterFromStringFunction,
} = require('../utils/parseParameterFromFunction')

const parseWhereAdvanced = (slicedCommandAsStringArray) => {
    const indexOfOrderBy = slicedCommandAsStringArray.find(
        (s) => s.toUpperCase() === 'ORDER'
    )

    const parsedWherePart = {
        keyword: slicedCommandAsStringArray[0],
        conditions:
            indexOfOrderBy > 0
                ? parseConditions(
                      slicedCommandAsStringArray.slice(1, indexOfOrderBy)
                  )
                : parseConditions(
                      slicedCommandAsStringArray.slice(
                          1,
                          slicedCommandAsStringArray.length - 1
                      )
                  ),
    }

    return parsedWherePart
}

const parseConditions = (slicedCommandArray) => {
    const conditionArray = prepareConditionsForParsing(slicedCommandArray)

    const conditions = { AND: [], OR: [] }

    const indexOfAnd = _.indexOf(conditionArray, 'AND')
    const indexOfOr = _.indexOf(conditionArray, 'OR')

    let AndOrSwitch = 'AND'

    if (indexOfAnd < 0 && indexOfOr >= 0) {
        AndOrSwitch = 'OR'
    } else if (indexOfAnd >= 0 && indexOfOr < 0) {
        AndOrSwitch = 'AND'
    } else {
        AndOrSwitch = indexOfAnd <= indexOfOr ? 'AND' : 'OR'
    }

    for (let i = 0; i < conditionArray.length; i++) {
        if (conditionArray[i] === '(') {
            const indexOfClosingBracket = findIndexOfClosingBracket(
                conditionArray,
                i
            )
            conditions[AndOrSwitch].push(
                parseConditions(
                    conditionArray.slice(i + 1, indexOfClosingBracket)
                )
            )
            i = indexOfClosingBracket + 1
        } else if (conditionArray[i] === 'AND') {
            AndOrSwitch = 'AND'
        } else if (conditionArray[i] === 'OR') {
            AndOrSwitch = 'OR'
        } else {
            const splitExpression = conditionArray[i].split(
                comparisonOperatorPattern
            )

            const condition = {
                left: parseConditionPart(splitExpression[0]),
                operator: splitExpression[1],
                right: parseConditionPart(splitExpression[2]),
            }
            conditions[AndOrSwitch].push(condition)
        }
    }

    return conditions
}

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

const findIndexOfClosingBracket = (conditionsArray, IndexOfOpeningBracket) => {
    let brackets = { opening: 0, closing: 0 }

    for (let i = IndexOfOpeningBracket; i < conditionsArray.length; i++) {
        if (conditionsArray[i] === '(') {
            brackets.opening = brackets.opening + 1
        } else if (conditionsArray[i] === ')') {
            brackets.closing = brackets.closing + 1
        }

        if (brackets.opening === brackets.closing) {
            return i
        }
    }
}

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

const parseConditionPart = (parsedField) => {
    switch (true) {
        case arithmeticExpressionPattern.test(parsedField):
            return {
                type: 'expression',
                value: parseExpression(parsedField),
                stringValue: parsedField,
            }
        case stringFunctionPattern.test(parsedField):
            return {
                type: 'stringFunction',
                name: parsedField.split('(')[0].toUpperCase(),
                value: parsedField,
                param: parseParameterFromStringFunction(parsedField),
            }
        case /^'\w+'/.test(parsedField):
            return {
                type: 'string',
                value: parsedField.replace(/'/g, ''),
            }
        case !isNaN(parsedField):
            return {
                type: 'integer',
                value: Number(parsedField),
            }
        default:
            return {
                type: 'column',
                value: parsedField,
            }
    }
}

const parseExpression = (expression) => {
    const splitExpression = expression.split(arithmeticOperatorPattern)

    return splitExpression.map((e) => parseExpressionFields(e))
}

const parseExpressionFields = (expressionElement) => {
    switch (true) {
        case stringFunctionPattern.test(expressionElement):
            return {
                type: 'stringFunction',
                name: expressionElement.split('(')[0].toUpperCase(),
                value: expressionElement,
                param: parseParameterFromStringFunction(expressionElement),
            }
        case /^'\w+'/.test(expressionElement):
            return {
                type: 'string',
                value: expressionElement.replace(/'/g, ''),
            }
        case !isNaN(expressionElement):
            return {
                type: 'integer',
                value: Number(expressionElement),
            }
        case /^[+\-*%]$/.test(expressionElement):
            return {
                type: 'operator',
                value: expressionElement,
            }
        default:
            return {
                type: 'column',
                value: expressionElement,
            }
    }
}

module.exports = { parseWhereAdvanced }
