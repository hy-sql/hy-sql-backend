const {
    stringFunctionsNamePattern,
    aggregateFunctionsNamePattern,
} = require('./regex')

const parseColumnFromStringFunction = (functionAsString) => {
    return functionAsString
        .replace(stringFunctionsNamePattern, '')
        .replace('(', '')
        .replace(')', '')
}

const parseColumnFromAggregateFunction = (functionAsString) => {
    return functionAsString
        .replace(aggregateFunctionsNamePattern, '')
        .replace('(', '')
        .replace(')', '')
}

module.exports = {
    parseColumnFromStringFunction,
    parseColumnFromAggregateFunction,
}
