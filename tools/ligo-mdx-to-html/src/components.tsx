export type syntax = "jsligo" | "cameligo";

const nice_blue = "rgb(114, 143, 203)";

const declarationStyle = {
  fontSize: "1.2em",
  fontWeight: "bold",
  marginTop: "1.5em",
  marginBottom: "0em",
  color: nice_blue,
  fontFamily:
    "SFMono-Regular, Menlo, Roboto Mono, Segoe UI, Courier, monospace",
};

export function Syntax(props: any, targetSyntax: syntax) {
  if (props.syntax === targetSyntax) {
    return props.children;
  } else {
    return null;
  }
}

export function SyntaxTitle(props: any, targetSyntax: syntax) {
  if (props.syntax === targetSyntax) {
    return <div style={declarationStyle}>{props.children}</div>;
  } else {
    return null;
  }
}

export function Spoiler(props: any, targetSyntax: syntax) {
  const summary =
    targetSyntax == "jsligo" ? props.summary_jsligo : props.summary_cameligo;
  return (
    <details>
      <summary style={{ ...declarationStyle, marginTop: "0em" }}>
        {summary}
      </summary>
      <div style={{ padding: "1em 2em" }}>{props.children}</div>
    </details>
  );
}

export function Box(props: any) {
  return (
    <div
      style={{
        border: "2px solid",
        borderColor: nice_blue,
        borderRadius: "8px",
        paddingLeft: "1em",
        paddingTop: "1em",
        marginTop: "1.5em",
        marginBottom: "1em",
      }}
    >
      {props.children}
    </div>
  );
}
