import { useEffect, useState } from "react";
import { WebIdeApi } from "~/components/api/api";

const LigoVersion = () => {
  const [version, setVersion] = useState("Version");
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    const loadVersion = async () => {
      await WebIdeApi.ligoVersion()
        .then((resp) => {
          setVersion(resp.data);
          setLoading(false);
        })
        .catch((e) => console.error(e));
    };
    // eslint-disable-next-line @typescript-eslint/no-floating-promises
    loadVersion();
  }, []);

  return !loading && version !== "Version" ? (
    <a
      href={`//ligolang.org/docs/intro/changelog/#${version.replaceAll(".", "")}`}
      target="_blank"
      rel="noreferrer"
    >
      <div className="btn btn-default btn-sm btn-flat text-muted">
        <i className="fas fa-cogs mr-1" />
        {`Ligo ${version}`}
      </div>
    </a>
  ) : null;
};

export default LigoVersion;
