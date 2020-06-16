const {
    containsFunctionPatternWithWhiteSpaces,
    comparisonOperatorPatternWithWhiteSpace,
} = require('../../helpers/regex')

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

const transformOrderByInputArrayIntoOrderByFieldsArray = (commandArray) => {
    const orderByFieldsArray = commandArray
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
    transformCommandArrayIntoConditionsArray,
    transformOrderByInputArrayIntoOrderByFieldsArray,
}
