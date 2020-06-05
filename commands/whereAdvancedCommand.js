const _ = require('lodash')
const {
    arithmeticExpressionPattern,
    comparisonOperatorPattern,
    stringFunctionExpressionPattern,
    endsWithStringFunctionExpressionPattern,
} = require('../utils/regex')
const {
    parseColumnsFromExpression,
} = require('../utils/parseColumnsFromExpression')
const {
    parseColumnFromStringFunction,
} = require('../utils/parseColumnFromFunction')

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

    const indexOfAnd = _.findIndex(conditionArray)
    const indexOfOr = _.findIndex(conditionArray)

    let AndOrSwitch = indexOfAnd < indexOfOr ? 'AND' : 'OR'

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
            const indexOfOperator = conditionArray[i].search(
                comparisonOperatorPattern
            )
            const splitExpression = conditionArray[i].split(
                comparisonOperatorPattern
            )
            const condition = {
                left: parseConditionPart(splitExpression[0]),
                operator: conditionArray[i][indexOfOperator],
                right: parseConditionPart(splitExpression[1]),
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
        if (!c.match(endsWithStringFunctionExpressionPattern)) {
            newArray.push(c.replace('(', ' ( ').replace(')', ' ) ').split(' '))
        } else {
            newArray.push(c)
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

const parseConditionPart = (parsedField) => {
    switch (true) {
        case arithmeticExpressionPattern.test(parsedField):
            return {
                type: 'expression',
                value: parsedField,
                columns: parseColumnsFromExpression(parsedField),
            }
        case stringFunctionExpressionPattern.test(parsedField):
            return {
                type: 'stringFunction',
                name: parsedField.split('(')[0].toUpperCase(),
                value: parsedField,
                column: parseColumnFromStringFunction(parsedField),
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

module.exports = { parseWhereAdvanced }
