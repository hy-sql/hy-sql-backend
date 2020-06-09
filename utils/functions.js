const _ = require('lodash')

const executeStringFunction = (functionDetails, row) => {
    switch (functionDetails.name) {
        case 'LENGTH':
            return functionDetails.param.type === 'column'
                ? row[functionDetails.param.value].length
                : functionDetails.param.value.length
        case 'CONCAT':
            return 'function not implemented yet'
        case 'SUBSTRING':
            return 'function not implemented yet'
    }
}

const executeAggregateFunction = (functionDetails, rows) => {
    switch (functionDetails.name) {
        case 'AVG':
            return 'function not implemented yet'
        case 'COUNT':
            return functionDetails.param === '*'
                ? rows.length
                : _.filter(rows, functionDetails.column).filter(Boolean).length
        case 'MAX':
            return 'function not implemented yet'
        case 'MIN':
            return 'function not implemented yet'
        case 'SUM':
            return 'function not implemented yet'
    }
}

module.exports = { executeStringFunction, executeAggregateFunction }
