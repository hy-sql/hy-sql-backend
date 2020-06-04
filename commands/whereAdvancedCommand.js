const { comparisonOperatorPattern } = require('../utils/regex')
const {
    arithmeticExpressionPattern,
    stringFunctionExpressionPattern,
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
                ? parseCondition(
                      slicedCommandAsStringArray.slice(1, indexOfOrderBy)
                  )
                : parseCondition(
                      slicedCommandAsStringArray.slice(
                          1,
                          slicedCommandAsStringArray.length - 1
                      )
                  ),
    }

    return parsedWherePart
}

const parseCondition = (slicedCommandArray) => {
    console.log(slicedCommandArray)

    const conditions = slicedCommandArray
        .filter((c) => c !== 'AND' && c !== 'OR')
        .map((c) => {
            console.log(c)
            const indexOfOperator = c.search(comparisonOperatorPattern)
            const splitExpression = c.split(comparisonOperatorPattern)

            return {
                left: parseConditionPart(splitExpression[0]),
                operator: c[indexOfOperator],
                right: parseConditionPart(splitExpression[1]),
            }
        })

    console.log(conditions)

    return conditions
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
