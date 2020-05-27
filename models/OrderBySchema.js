const Joi = require('@hapi/joi')

const OrderBySchema = Joi.object({
    keyword: Joi.string()
        .pattern(/[;]/, { invert: true })
        .pattern(/^ORDER BY$/)
        .insensitive()
        .optional()
        .messages({}),

    columnName: Joi.string()
        .pattern(/[;]/, { invert: true })
        .pattern(/^\w+$/)
        .max(64)
        .required()
        .messages({
            'string.pattern.invert.base':
                'Semicolon should be only found at the end of a query',
            'string.pattern.base':
                'Only letters, numbers and underscore allowed',
        }),

    order: Joi.string()
        .pattern(/[;]/, { invert: true })
        .pattern(/^ASC$|^DESC$/i)
        .insensitive()
        .max(64)
        .optional()
        .messages({
            'string.pattern.invert.base':
                'Semicolon should be only found at the end of a query',
            'string.pattern.base':
                'Not a sorting keyword at the end of the query',
        }),
})

module.exports = OrderBySchema
