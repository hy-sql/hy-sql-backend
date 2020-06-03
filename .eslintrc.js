module.exports = {
    env: {
        commonjs: true,
        es6: true,
        node: true,
        jest: true,
    },
    extends: ['eslint:recommended', 'prettier'],
    globals: {
        Atomics: 'readonly',
        SharedArrayBuffer: 'readonly',
    },
    parserOptions: {
        ecmaVersion: 11,
    },
    plugins: ['prettier'],
    rules: {
        'prettier/prettier': 'error',
        'linebreak-style': ['error', 'unix'],
        quotes: ['error', 'single', { avoidEscape: true }],
        semi: ['error', 'never'],
        eqeqeq: 'error',
        'no-trailing-spaces': 'error',
        'object-curly-spacing': ['error', 'always'],
        'arrow-spacing': ['error', { before: true, after: true }],
    },
}
