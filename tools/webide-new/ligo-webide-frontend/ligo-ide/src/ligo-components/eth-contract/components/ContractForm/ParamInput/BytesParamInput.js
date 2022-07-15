import React from "react";

import { DebouncedInput, Badge } from "~/base-components/ui-components";

import { networkManager } from "~/ligo-components/eth-network";

export default function BytesParamInput({
  size,
  label,
  type,
  value,
  onChange,
  placeholder,
  disabled,
}) {
  const { raw = "", encoding = "hex" } = value || {};

  let invalid;
  let feedback;
  if (encoding === "hex") {
    invalid = raw && !/^0[xX][0-9a-fA-F]+$/.test(raw);
    feedback = invalid && "Invalid hex string";
  }

  let typeLength;
  if (type === "byte") {
    typeLength = 1;
  } else if (type.substr(5)) {
    typeLength = Number(type.substr(5));
  }

  const onChangeValue = (raw, encoding) => {
    let bytes;
    if (encoding === "hex") {
      let hex = raw.toLowerCase();
      if (!hex.startsWith("0x")) {
        hex = `0x${hex}`;
      }
      try {
        bytes = networkManager.sdk?.utils.format.bytesFromHex(hex);
      } catch {
        onChange(
          { encoding, raw },
          {
            error: new Error(`Not a valid hex string for parameter <b>${label}</b>.`),
          }
        );
        return;
      }
    } else {
      bytes = networkManager.sdk?.utils.format.bytes(raw);
    }

    const length = typeLength || bytes.length;
    if (bytes.length > length) {
      onChange(
        { encoding, raw },
        {
          error: new Error(
            `Byte length overflow for parameter <b>${label}</b>. Expect ${length} but got ${bytes.length}.`
          ),
        }
      );
      return;
    }
    const arr = new Uint8Array(length);
    arr.set(bytes);
    onChange(
      { encoding, raw },
      {
        display: `0x${Buffer.from(arr).toString("hex")}`,
        raw: arr,
        empty: !bytes.length,
      }
    );
  };

  React.useEffect(() => {
    onChangeValue(raw, encoding);
  }, []);

  return (
    <div style={{ position: "relative" }}>
      <DebouncedInput
        type={!typeLength || typeLength > 8 ? "textarea" : "text"}
        className="rounded"
        size={size}
        value={raw}
        onChange={raw => onChangeValue(raw, encoding)}
        placeholder={placeholder}
        maxLength={typeLength && (encoding === "utf8" ? typeLength : 2 * typeLength + 2)}
        disabled={disabled}
        feedback={feedback}
        invalid={invalid}
      >
        <Badge
          color={encoding === "hex" ? "primary" : "secondary"}
          style={{
            position: "absolute",
            right: "45px",
            bottom: "5px",
            height: "18px",
            zIndex: 100,
            borderTopRightRadius: 0,
            borderBottomRightRadius: 0,
          }}
          onClick={() => onChangeValue(raw, "hex")}
        >
          HEX
        </Badge>
        <Badge
          color={encoding === "utf8" ? "primary" : "secondary"}
          style={{
            position: "absolute",
            right: "5px",
            bottom: "5px",
            height: "18px",
            zIndex: 100,
            borderTopLeftRadius: 0,
            borderBottomLeftRadius: 0,
          }}
          onClick={() => onChangeValue(raw, "utf8")}
        >
          UTF8
        </Badge>
      </DebouncedInput>
    </div>
  );
}
