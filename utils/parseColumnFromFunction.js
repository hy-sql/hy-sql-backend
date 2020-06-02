const { stringFunctionsPattern } = require('./regex')

const parseColumnFromFunction = (func) => {
    console.log(func)
    return func
        .replace(stringFunctionsPattern, '')
        .replace('(', '')
        .replace(')', '')
}

module.exports = { parseColumnFromFunction }
