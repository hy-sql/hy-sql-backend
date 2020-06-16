const Joi = require('@hapi/joi')
const {
    allFunctionsNamePattern,
    aggregateFunctionPattern,
    stringFunctionPattern,
} = require('../helpers/regex')
const {
    AllFieldsSchema,
    ColumnSchema,
    IntegerSchema,
    TextSchema,
} = require('./FieldSchemas')

/**
 * Joi schema for validating functions.
 */
const FunctionSchema = Joi.object({
    // Works for now, have to think about it later

    type: Joi.string().valid('stringFunction', 'aggregateFunction').required(),
    name: Joi.string().pattern(allFunctionsNamePattern).required(),
    value: Joi.when('type', {
        switch: [
            {
                is: 'stringFunction',
                then: Joi.string().pattern(stringFunctionPattern).required(),
            },
            {
                is: 'aggregateFunction',
                then: Joi.string().pattern(aggregateFunctionPattern).required(),
            },
        ],
    }),
    param: Joi.alternatives()
        .try(
            AllFieldsSchema,
            ColumnSchema,
            TextSchema,
            IntegerSchema,
            Joi.link('#expression')
        )
        .required(),
}).id('function')

module.exports = FunctionSchema
