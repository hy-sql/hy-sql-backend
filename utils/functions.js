const _ = require('lodash')

const executeStringFunction = (functionDetails, row) => {
    switch (functionDetails.name) {
        case 'LENGTH':
            return functionDetails.param.type === 'column'
                ? row[functionDetails.param.value].toString().length
                : functionDetails.param.value.toString().length
        case 'CONCAT':
            return 'function not implemented yet'
        case 'SUBSTRING':
            return 'function not implemented yet'
    }
}

const executeAggregateFunction = (functionDetails, rows) => {
    console.log('My Function', functionDetails)
    switch (functionDetails.name) {
        case 'AVG':
            return 'function not implemented yet'
        case 'COUNT':
            return functionDetails.param.type === 'all'
                ? rows.length
                : _.filter(rows, functionDetails.param.value).filter(Boolean)
                      .length
        case 'MAX':
            return 'function not implemented yet'
        case 'MIN':
            return 'function not implemented yet'
        case 'SUM':
            return 'function not implemented yet'
    }
}

module.exports = { executeStringFunction, executeAggregateFunction }
