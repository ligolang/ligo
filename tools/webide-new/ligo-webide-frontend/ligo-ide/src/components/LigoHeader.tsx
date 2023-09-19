import "./styles.scss";
import ligologo from "./logo.svg";

const links = [
  {
    link: "ligolang.org/docs/intro/introduction",
    name: "Docs",
    isBlank: true,
    isCurrent: false,
  },
  { link: "ligolang.org/docs/reference/toplevel", name: "API", isBlank: true, isCurrent: false },
  {
    link: "ligolang.org/docs/faq/intro",
    name: "FAQ",
    isBlank: true,
    isCurrent: false,
  },
];

const LigoHeader = () => {
  return (
    <div className="ligoNavbar">
      <div className="ligoGroup">
        <a
          className="ligoLink ligoLinkNoPadding"
          href="////ligolang.org"
          target="_blank"
          rel="noreferrer"
        >
          <img className="ligoLogo" src={ligologo} alt="Hello" />
        </a>
        {links.map((l) => {
          return (
            <a
              className={`ligoLink${l.isCurrent ? " ligoLinkActive" : ""}`}
              href={`////${l.link}`}
              target={l.isBlank ? "_blank" : "_self"}
              rel="noreferrer"
            >
              {l.name}
            </a>
          );
        })}
      </div>
      <a
        className="ligoLink ligoLinkCheat"
        href="////ligolang.org/docs/api/cheat-sheet"
        target="_blank"
        rel="noreferrer"
      >
        Cheat Sheet
      </a>
    </div>
  );
};

export default LigoHeader;
