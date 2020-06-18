const Joi = require('@hapi/joi')
const { ColumnSchema } = require('./FieldSchemas')
const ExpressionSchema = require('./ExpressionSchema')
const FunctionSchema = require('./FunctionSchema')

/**
 * Joi schema for validating DISTINCT keyword objects
 */
const DistinctSchema = Joi.object({
    type: Joi.string().valid('distinct').required(),
    value: Joi.array().items(ColumnSchema, ExpressionSchema, FunctionSchema),
})

module.exports = DistinctSchema
