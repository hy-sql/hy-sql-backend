const {
    stringFunctionsNamePattern,
    aggregateFunctionsNamePattern,
} = require('./regex')

const parseParameterFromStringFunction = (functionAsString) => {
    return parseExpressionFields(
        functionAsString
            .replace(stringFunctionsNamePattern, '')
            .replace('(', '')
            .replace(')', '')
    )
}

const parseParameterFromAggregateFunction = (functionAsString) => {
    return parseExpressionFields(
        functionAsString
            .replace(aggregateFunctionsNamePattern, '')
            .replace('(', '')
            .replace(')', '')
    )
}

const parseExpressionFields = (expressionElement) => {
    switch (true) {
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
        default:
            return {
                type: 'column',
                value: expressionElement,
            }
    }
}

module.exports = {
    parseParameterFromStringFunction,
    parseParameterFromAggregateFunction,
}
