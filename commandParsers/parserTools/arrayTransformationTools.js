const {
    containsFunctionPatternWithWhiteSpaces,
    comparisonOperatorPatternWithWhiteSpace,
} = require('../../helpers/regex')

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
const transformSplitConditionsIntoConditionsArray = (
    slicedConditionsPartOfCommand
) => {
    const conditionsArray = slicedConditionsPartOfCommand
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
    transformSplitConditionsIntoConditionsArray,
    transformOrderByInputArrayIntoOrderByFieldsArray,
}
