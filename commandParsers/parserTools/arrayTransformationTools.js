const {
    fieldsSplitByCommaPattern,
    containsFunctionPatternWithWhiteSpaces,
    comparisonOperatorPatternWithWhiteSpace,
    arithmeticOperatorPatternWithWhiteSpace,
} = require('../../helpers/regex')
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
const transformSelectInputArrayIntoFieldsArray = (selectInputArray) => {
    const selectFieldsAsString = selectInputArray
        .join(' ')
        .replace(containsFunctionPatternWithWhiteSpaces, (m) =>
            m.replace(/\s+/g, '')
        )
        .replace(arithmeticOperatorPatternWithWhiteSpace, (m) =>
            m.replace(/\s+/g, '')
        )
        .replace(/\s+,/g, (m) => m.replace(/\s+/g, ''))
        .trim()

    if (!fieldsSplitByCommaPattern.test(selectFieldsAsString)) {
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
    const conditionsArray = conditionsInputArray
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
    const orderByFieldsArray = splitOrderByFields
        .join(' ')
        .replace(containsFunctionPatternWithWhiteSpaces, (m) =>
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
