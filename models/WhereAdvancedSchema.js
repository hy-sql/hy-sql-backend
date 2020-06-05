const Joi = require('@hapi/joi')

const WhereAdvancedSchema = Joi.object({
    keyword: Joi.string()
        .required()
        .pattern(/;/, { invert: true })
        .pattern(/^([Ww][Hh][Ee][Rr][Ee])$/)
        .messages({}),

    conditions: Joi.object(),
})

module.exports = WhereAdvancedSchema
