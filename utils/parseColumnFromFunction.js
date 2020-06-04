const { stringFunctionsPattern, aggregateFunctionsPattern } = require('./regex')

const parseColumnFromStringFunction = (functionAsString) => {
    return functionAsString
        .replace(stringFunctionsPattern, '')
        .replace('(', '')
        .replace(')', '')
}

const parseColumnFromAggregateFunction = (functionAsString) => {
    return functionAsString
        .replace(aggregateFunctionsPattern, '')
        .replace('(', '')
        .replace(')', '')
}

module.exports = {
    parseColumnFromStringFunction,
    parseColumnFromAggregateFunction,
}
