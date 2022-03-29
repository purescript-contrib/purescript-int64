import L from './long.js/index.js';

// Constants
export const zero     = L.ZERO;
export const one      = L.ONE;
export const negOne   = L.NEG_ONE;
export const uzero    = L.UZERO;
export const uone     = L.UONE;
export const maxValue = L.MAX_VALUE;
export const maxUnsignedValue = L.MAX_UNSIGNED_VALUE;
export const minValue = L.MIN_VALUE;

// Utilities
export const isLong = L.isLong;
export const fromBits = L.fromBits;
export const fromBytes = L.fromBytes;
export const fromBytesLE = L.fromBytesLE;
export const fromBytesBE = L.fromBytesBE;
export const fromInt = L.fromInt;
export const fromNumber = L.fromNumber;
export const fromString = L.fromString;
export const fromValue = L.fromValue;

// Fields
export const unsigned = function (l) { return l.unsigned; };

// Methods
export const add = function (l) { return l.add.bind(l); };
export const and = function (l) { return l.and.bind(l); };
export const compare = function (l) { return l.compare.bind(l); };
export const divide = function (l) { return l.divide.bind(l); };
export const equals = function (l) { return l.equals.bind(l); };
export const getHighBits = function (l) { return l.getHighBits(); };
export const getHighBitsUnsigned = function (l) { return l.getHighBitsUnsigned(); };
export const getLowBits = function (l) { return l.getLowBits(); };
export const getLowBitsUnsigned = function (l) { return l.getLowBitsUnsigned(); };
export const getNumBitsAbs = function (l) { return l.getNumBitsAbs(); };
export const greaterThan = function (l) { return l.greaterThan.bind(l); };
export const greaterThanOrEqual = function (l) { return l.greaterThanOrEqual.bind(l); };
export const isEven = function (l) { return l.isEven(); };
export const isNegative = function (l) { return l.isNegative(); };
export const isOdd = function (l) { return l.isOdd(); };
export const isPositive = function (l) { return l.isPositive(); };
export const isZero = function (l) { return l.isZero(); };
export const lessThan = function (l) { return l.lessThan.bind(l); };
export const lessThanOrEqual = function (l) { return l.lessThanOrEqual.bind(l); };
export const modulo = function (l) { return l.modulo.bind(l); };
export const multiply = function (l) { return l.multiply.bind(l); };
export const negate = function (l) { return l.negate(); };
export const not = function (l) { return l.not(); };
export const notEquals = function (l) { return l.notEquals.bind(l); };
export const or = function (l) { return l.or.bind(l); };
export const shiftLeft = function (l) { return l.shiftLeft.bind(l); };
export const shiftRight = function (l) { return l.shiftRight.bind(l); };
export const shiftRightUnsigned = function (l) { return l.shiftRightUnsigned.bind(l); };
export const rotateLeft = function (l) { return l.rotateLeft.bind(l); };
export const rotateRight = function (l) { return l.rotateRight.bind(l); };
export const subtract = function (l) { return l.subtract.bind(l); };
export const toBytes = function (l) { return l.toBytes.bind(l); };
export const toInt = function (l) { return l.toInt(); };
export const toNumber = function (l) { return l.toNumber(); };
export const toSigned = function (l) { return l.toSigned(); };
export const toString = function (l) { return l.toString.bind(l); };
export const toUnsigned = function (l) { return l.toUnsigned(); };
export const xor = function (l) { return l.xor.bind(l); };
