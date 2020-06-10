const _ = require('lodash')
const { comparisonOperatorPattern } = require('../../helpers/regex')
const parseField = require('./fieldParser')
const prepareConditionsForParsing = require('../parserTools/prepareConditionsForParsing')
const findIndexOfClosingBracket = require('../parserTools/findIndexOfClosingBracket')

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
                left: parseField(splitExpression[0]),
                operator: splitExpression[1],
                right: parseField(splitExpression[2]),
            }
            conditions[AndOrSwitch].push(condition)
        }
    }

    return conditions
}

module.exports = parseConditions
