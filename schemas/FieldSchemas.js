const Joi = require('@hapi/joi')
const {
    allFunctionsNamePattern,
    arithmeticOperator,
    sortOrderKeywordPattern,
} = require('../helpers/regex')

const AllFieldsSchema = Joi.object({
    type: Joi.string().valid('all').required(),
    value: Joi.string().valid('*').required(),
})

const ColumnSchema = Joi.object({
    type: Joi.string().valid('column').required(),
    value: Joi.string()
        .pattern(/^\w+$/)
        .pattern(allFunctionsNamePattern, { invert: true })
        .required(),
})

const TextSchema = Joi.object({
    type: Joi.string().valid('text').required(),
    value: Joi.string().required(),
})

const IntegerSchema = Joi.object({
    type: Joi.string().valid('integer').required(),
    value: Joi.number().required(),
})

const ArithmeticOperatorSchema = Joi.object({
    type: Joi.string().valid('operator').required(),
    value: Joi.string().pattern(arithmeticOperator).required(),
})

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
