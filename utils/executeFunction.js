const _ = require('lodash')
const { parseColumnFromFunction } = require('./parseColumnFromFunction')

const executeStringFunction = (functionField, columnValue) => {
    switch (functionField.name) {
        case 'LENGTH':
            return columnValue.length
        case 'CONCAT':
            return 'function not implemented yet'
        case 'SUBSTRING':
            return 'function not implemented yet'
    }
}

const executeAggregateFunction = (functionField, rows) => {
    const columnToOperateOn = parseColumnFromFunction(functionField)
    switch (functionField.name) {
        case 'AVG':
            return 'function not implemented yet'
        case 'COUNT':
            return columnToOperateOn === '*'
                ? rows.length
                : _.filter(rows, columnToOperateOn).filter(Boolean).length
        case 'MAX':
            return 'function not implemented yet'
        case 'MIN':
            return 'function not implemented yet'
        case 'SUM':
            return 'function not implemented yet'
    }
}

module.exports = { executeStringFunction, executeAggregateFunction }
