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
    switch (functionDetails.name) {
        case 'AVG':
            return _.meanBy(rows, functionDetails.param.value)
        case 'COUNT':
            return functionDetails.param.type === 'all'
                ? rows.length
                : _.filter(rows, functionDetails.param.value).filter(Boolean)
                      .length
        case 'MAX':
            return _.maxBy(rows, functionDetails.param.value)
        case 'MIN':
            return _.minBy(rows, functionDetails.param.value)
        case 'SUM':
            return _.sumBy(rows, functionDetails.param.value)
    }
}

module.exports = { executeStringFunction, executeAggregateFunction }
