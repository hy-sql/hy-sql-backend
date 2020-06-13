const Joi = require('@hapi/joi')

const OrderBySchema = Joi.object({
    keyword: Joi.string()
        .pattern(/[;]/, { invert: true })
        .pattern(/^ORDER BY$/i)
        .messages({
            'any.required':
                'This query is expected to contain the following keyword: ORDER BY',
            'string.pattern.invert.base':
                'Semicolon should only be found at the end of a query',
            'string.pattern.base':
                'ORDER BY is either misspelled, missing or in the wrong position',
        }),

    columns: Joi.array(),

    order: Joi.string()
        .pattern(/[;]/, { invert: true })
        .pattern(/^ASC.+$|^DESC.+$/i, { name: 'extra', invert: true })
        .pattern(/^ASC$|^DESC$/i)
        .allow('')
        .insensitive()
        .max(64)
        .optional()
        .messages({
            'string.pattern.invert.base':
                'Semicolon should be only found at the end of a query',
            'string.pattern.base':
                'Not a sorting keyword at the end of the query',
            'string.pattern.invert.name':
                'Extra characters at the end of the line are causing command to fail',
        }),
})

module.exports = OrderBySchema
