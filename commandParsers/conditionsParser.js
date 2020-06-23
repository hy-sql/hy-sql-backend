const _ = require('lodash')
const { containsComparisonOperatorPattern } = require('../helpers/regex')
const { containsComparisonOperator } = require('../helpers/containsRegexTools')
const findIndexOfClosingBracket = require('./parserTools/findIndexOfClosingBracket')
const {
    transformSplitConditionsIntoConditionsArray,
} = require('./parserTools/arrayTransformationTools')
const { parseField } = require('./fieldsParser')

/**
 * Handles parsing of conditions from the given array.
 * @param {string[]} slicedCommandArray array containing the conditions
 */
const parseConditions = (slicedCommandArray) => {
    const conditionArray = transformSplitConditionsIntoConditionsArray(
        slicedCommandArray
    )

    const conditions = { AND: [], OR: [] }

    const indexOfAnd = _.indexOf(conditionArray, 'AND')
    const indexOfOr = _.indexOf(conditionArray, 'OR')

    let AndOrSwitch = indexOfOr < (indexOfAnd >= 0) ? 'AND' : 'OR'

    for (let i = 0; i < conditionArray.length; i++) {
        switch (conditionArray[i]) {
            case '(': {
                const indexOfClosingBracket = findIndexOfClosingBracket(
                    conditionArray,
                    i
                )

                conditions[AndOrSwitch] = conditions[AndOrSwitch].concat(
                    parseConditions(
                        conditionArray.slice(i + 1, indexOfClosingBracket)
                    )
                )

                i = indexOfClosingBracket
                break
            }
            case ')':
                break
            case 'AND':
                AndOrSwitch = 'AND'
                break
            case 'OR':
                AndOrSwitch = 'OR'
                break
            default: {
                if (containsComparisonOperator(conditionArray[i])) {
                    const splitExpression = conditionArray[i].split(
                        containsComparisonOperatorPattern
                    )

                    const condition = {
                        left: parseField(splitExpression[0]),
                        operator: splitExpression[1],
                        right: parseField(splitExpression[2]),
                    }

                    conditions[AndOrSwitch] = conditions[AndOrSwitch].concat(
                        condition
                    )
                } else {
                    conditions[AndOrSwitch] = conditions[AndOrSwitch].concat(
                        conditionArray[i]
                    )
                }
            }
        }
    }

    return conditions
}

module.exports = { parseConditions }
