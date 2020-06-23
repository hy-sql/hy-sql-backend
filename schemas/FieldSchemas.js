const Joi = require('@hapi/joi')
const {
    allFunctionsNamePattern,
    arithmeticOperatorPattern,
    sortOrderKeywordPattern,
} = require('../helpers/regex')

/**
 * Joi schema for validating field objects of fields that are supposed to contain *.
 */
const AllFieldsSchema = Joi.object({
    type: Joi.string().valid('all').required(),
    value: Joi.string().valid('*').required(),
})

/**
 * Joi schema for validating column objects.
 */
const ColumnSchema = Joi.object({
    type: Joi.string().valid('column').required(),
    value: Joi.string()
        .pattern(/^\w+$/)
        .pattern(allFunctionsNamePattern, { invert: true })
        .invalid('FROM')
        .insensitive()
        .max(64)
        .required(),
})

/**
 * Joi schema for validating field objects of type text.
 */
const TextSchema = Joi.object({
    type: Joi.string().valid('text').required(),
    value: Joi.string().required(),
})

/**
 * Joi schema for validating field objects of type integer.
 */
const IntegerSchema = Joi.object({
    type: Joi.string().valid('integer').required(),
    value: Joi.number().required().messages({
        'any.required':
            'value type without singlequotes is expected to be number',
    }),
})

/**
 * Joi schema for validating arithmetic operator objects.
 */
const ArithmeticOperatorSchema = Joi.object({
    type: Joi.string().valid('operator').required(),
    value: Joi.string().pattern(arithmeticOperatorPattern).required(),
})

/**
 * Joi schema for validating ASC and DESC keyword objects.
 */
const SortOrderKeyword = Joi.object({
    type: Joi.string().valid('order').required(),
    value: Joi.string().pattern(sortOrderKeywordPattern).required(),
})

module.exports = {
    AllFieldsSchema,
    ArithmeticOperatorSchema,
    ColumnSchema,
    IntegerSchema,
    SortOrderKeyword,
    TextSchema,
}
