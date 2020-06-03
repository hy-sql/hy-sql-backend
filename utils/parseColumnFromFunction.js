const { stringFunctionsPattern, aggregateFunctionsPattern } = require('./regex')

const parseColumnFromFunction = (functionField) => {
    switch (functionField.type) {
        case 'stringFunction':
            return functionField.value
                .replace(stringFunctionsPattern, '')
                .replace('(', '')
                .replace(')', '')
        case 'aggregateFunction':
            return functionField.value
                .replace(aggregateFunctionsPattern, '')
                .replace('(', '')
                .replace(')', '')
    }
}

module.exports = { parseColumnFromFunction }
