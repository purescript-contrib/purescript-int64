import * as FFI from '../Data.Int64.Internal.FFI/foreign.js';

export const numberBitsToInt = function(x) {
  return x|0;
};

var radixCheckers = {};

function RadixChecker(radix) {
  var digits;
  if (radix < 11) {
    digits = "[0-" + (radix - 1).toString() + "]";
  } else if (radix === 11) {
    digits = "[0-9a]";
  } else {
    digits = "[0-9a-" + String.fromCharCode(86 + radix) + "]";
  }

  this.baseRE = new RegExp("^" + digits + "+$", "i");

  this.maxNegSignedBase = FFI.MIN_VALUE.toString(radix).substring(1);
  this.maxPosSignedBase = FFI.MAX_VALUE.toString(radix);
  this.maxUnsignedBase = FFI.MAX_UNSIGNED_VALUE.toString(radix);
}

function hasValidDigits(isNegative, isUnsigned, base, radix) {
  radixCheckers[radix] = radixCheckers[radix] || new RadixChecker(radix);
  var rc = radixCheckers[radix];

  var maxBase;
  if (isUnsigned) {
    maxBase = rc.maxUnsignedBase;
  } else if (isNegative) {
    maxBase = rc.maxNegSignedBase;
  } else {
    maxBase = rc.maxPosSignedBase;
  }

  return ((base.length < maxBase.length)
          || (base.length === maxBase.length && base <= maxBase)
         ) && rc.baseRE.test(base);
}

export const _safeReadLong = function(s, isUnsigned, radix) {
  var isNegative = s.startsWith("-");
  var hasPositivePrefix = s.startsWith("+");

  var base = (isNegative || hasPositivePrefix) ? s.substring(1) : s;

  // remove preceding zeros
  var lastNdx = 0;
  while (lastNdx < base.length - 1 && base[lastNdx] === "0") { lastNdx++; }
  base = base.substring(lastNdx);

  var retVal = null;
  if (
    // long.js return 0 for empty string
    (base !== "")
    // long.js coerces negative values to their unsigned counterparts
      && (!isNegative || !isUnsigned || base === "0")
    // Invalid characters / overflow
      && hasValidDigits(isNegative, isUnsigned, base, radix)
  ) {
    try {
      var signPrefix = isNegative ? "-" : "";

      // long.js may throw errors
      retVal = FFI.fromString(signPrefix + base, isUnsigned, radix);
    } catch (err) {
    }
  }

  return retVal;
};

export const isWholeNumber = function(n) {
  return Number.isInteger(n);
};
