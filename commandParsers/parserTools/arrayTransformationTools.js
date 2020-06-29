const {
    containsArithmeticOperatorWithWhiteSpacePattern,
    containsComparisonOperatorWithWhiteSpacePattern,
    containsFunctionWithWhiteSpacesPattern,
} = require('../../helpers/regex')
const { fieldsSplitByComma } = require('../../helpers/isRegexTools')
const SQLError = require('../../models/SQLError')

/**
 * Transforms select command split with splitCommandIntoArray into individual select fields
 * If fields are not divided by comma, error is thrown
 * @param {Array} selectInputArray
 *
 * const input = [ 'name,', 'price,', 'LENGTH', '(', 'name', ')', '*2+price' ]
 *
 * const output = transformSelectInputArrayIntoFieldsArray(input)
 * output is [ 'name', 'price', 'LENGTH(name)*2+price' ]
 */
const transformSelectInputArrayIntoFieldsArray = (inputArray) => {
    if (inputArray.length === 0) {
        return inputArray
    }

    const selectFieldsAsString = inputArray
        .join(' ')
        .replace(containsFunctionWithWhiteSpacesPattern, (m) =>
            m.replace(/\s+/g, '')
        )
        .replace(containsArithmeticOperatorWithWhiteSpacePattern, (m) =>
            m.replace(/\s+/g, '')
        )
        .replace(/\s+,/g, (m) => m.replace(/\s+/g, ''))
        .trim()

    if (!fieldsSplitByComma(selectFieldsAsString)) {
        throw new SQLError('fields must be split by comma (,)')
    }

    const selectFieldsArray = selectFieldsAsString.split(', ').filter(Boolean)

    return selectFieldsArray
}

/**
 * Transforms conditions part of command split by splitCommandIntoArray into array of individual conditions
 * @param {Array} slicedConditionsPartOfCommand
 *
 * const input = [
 * 'LENGTH', '(',       'nimi',
 * ')',      '=5',      'OR',
 * '(',      'LENGTH',  '(',
 * 'nimi',   ')',       '<7',
 * 'AND',    'hinta=4', ')'
 * ]
 *
 * const output = transformSplitConditionsIntoConditionsArray(slicedConditionsPartOfCommand)
 * output is [ 'LENGTH(nimi)=5', 'OR', '(', 'LENGTH(nimi)<7', 'AND', 'hinta=4', ')' ]
 */
const transformSplitConditionsIntoConditionsArray = (conditionsInputArray) => {
    if (conditionsInputArray.length === 0) {
        return conditionsInputArray
    }

    const conditionsArray = conditionsInputArray
        .join(' ')
        .replace(containsFunctionWithWhiteSpacesPattern, (m) =>
            m.replace(/\s+/g, '')
        )
        .replace(containsComparisonOperatorWithWhiteSpacePattern, (m) =>
            m.replace(/\s+/g, '')
        )
        .replace(containsArithmeticOperatorWithWhiteSpacePattern, (m) =>
            m.replace(/\s+/g, '')
        )
        .replace(/AND/gi, ' AND ')
        .replace(/OR/gi, ' OR ')
        .match(/(?:[^\s"']+|['"][^'"]*["'])+|'/g)

    return conditionsArray
}

/**
 * Transforms fields part of order by command split by splitCommandIntoArray into an array of arrays containing individual field parts
 * @param {Array} commandArray
 *
 * const input = [ 'LENGTH', '(', 'nimi', ')', 'DESC,', 'hinta' ]
 *
 * const output = transformOrderByInputArrayIntoOrderByFieldsArray(input)
 * output is [ [ 'LENGTH(nimi)', 'DESC' ], [ 'hinta' ] ]
 */
const transformOrderByInputArrayIntoOrderByFieldsArray = (
    splitOrderByFields
) => {
    if (splitOrderByFields.length === 0) {
        return splitOrderByFields
    }

    const orderByFieldsAsStringWithoutAscDesc = splitOrderByFields
        .join(' ')
        .replace(containsFunctionWithWhiteSpacesPattern, (m) =>
            m.replace(/\s+/g, '')
        )
        .replace(/ASC|DESC/gi, '')
        .replace(/\s+,/g, (m) => m.replace(/\s+/g, ''))
        .trim()

    if (!fieldsSplitByComma(orderByFieldsAsStringWithoutAscDesc)) {
        throw new SQLError('fields must be split by comma (,)')
    }

    const orderByFieldsArray = splitOrderByFields
        .join(' ')
        .replace(containsFunctionWithWhiteSpacesPattern, (m) =>
            m.replace(/\s+/g, '')
        )
        .split(',')
        .map((e) => e.trim())
        .map((f) => f.split(' '))

    return orderByFieldsArray
}

module.exports = {
    transformSelectInputArrayIntoFieldsArray,
    transformSplitConditionsIntoConditionsArray,
    transformOrderByInputArrayIntoOrderByFieldsArray,
}
