import { useState, forwardRef, useRef, useImperativeHandle } from "react";

import { Modal, DebouncedFormGroup, DropdownInput } from "~/base-components/ui-components";

import notification from "~/base-components/notification";

import { ProjectManager } from "../ProjectManager";
import actions from "../actions";
import { WebIdeApi } from "~/components/api/api";
import { validName } from "~/components/validators";

type Template = {
  id: string;
  gitLink?: string;
  display: string;
  existingSyntaxes: string[];
};

const synIds = ["mligo", "ligo", "jsligo"];
const hardcodedTemplates = [
  { id: "empty", gitLink: undefined, display: "Empty Project", existingSyntaxes: synIds },
  { id: "increment", gitLink: undefined, display: "Increment", existingSyntaxes: synIds },
  { id: "id", gitLink: undefined, display: "ID", existingSyntaxes: synIds },
  { id: "hashlock", gitLink: undefined, display: "Hashlock Contract", existingSyntaxes: synIds },
];
const pSyntaxes = [
  { id: "mligo", display: "CameLIGO" },
  { id: "ligo", display: "PascaLIGO" },
  { id: "jsligo", display: "JsLIGO" },
];

const mapSyntaxes = (s: string) => {
  if (s === "cameligo") {
    return "mligo";
  }
  if (s === "jsligo") {
    return "jsligo";
  }
  if (s === "pascaligo") {
    return "ligo";
  }
  return s;
};

const convertLigoTemplates = (templates: string[]) => {
  const syntaxPrefixes = ["cameligo", "jsligo", "pascaligo"];
  const splittedTemplates = templates.map((t) => t.split("-"));

  const s = new Set();
  const resultingTemplates: Template[] = [];

  splittedTemplates.forEach((t) => {
    const splittedId = t.filter((v, i) => i !== t.length - 1 || !syntaxPrefixes.includes(v));
    const id = splittedId.join("-");
    const templateSyntax = splittedId.length === t.length ? undefined : t[t.length - 1];
    if (!s.has(id)) {
      s.add(id);
      resultingTemplates.push({
        id,
        gitLink: id + (templateSyntax ? `-${templateSyntax}` : ""),
        display: splittedId[0].charAt(0).toUpperCase() + splittedId.join(" ").slice(1),
        existingSyntaxes: [],
      });
    }
    if (templateSyntax) {
      resultingTemplates.forEach((l) => {
        if (l.id === id) {
          l.existingSyntaxes.push(mapSyntaxes(templateSyntax));
        }
      });
    }
  });

  return resultingTemplates;
};

const NewProjectModal = forwardRef((_, ref) => {
  const [name, setName] = useState("");
  const [template, setTemplate] = useState("empty");
  const [templates, setTemplates] = useState<Template[]>(hardcodedTemplates);
  const [creating, setCreating] = useState(false);
  const [syntax, setSyntax] = useState("mligo");
  const [syntaxes, setSyntaxes] = useState(pSyntaxes);

  actions.newProjectModal = ref;
  const modalRef = useRef<Modal>(null);
  const resolverRef = useRef<
    ((_: { id: string; author: string; path: string; name: string }) => void) | undefined
  >(undefined);

  useImperativeHandle(ref, () => ({
    async openModal() {
      notification.info("Loading templates");
      const ligoTemplates = await WebIdeApi.listTemplates().catch((e: Error) => {
        notification.error("Templates Loading Error", e.message);
      });
      if (ligoTemplates) {
        setTemplates([...hardcodedTemplates, ...convertLigoTemplates(ligoTemplates.data)]);
      } else {
        setTemplates([...hardcodedTemplates]);
      }
      setName("");
      setTemplate("empty");
      setCreating(false);
      setSyntax("mligo");
      setSyntaxes(pSyntaxes);
      await modalRef.current?.openModal();
      return new Promise((resolve) => {
        resolverRef.current = resolve;
      });
    },
  }));

  const onCreateProject = async () => {
    setCreating(true);

    const templateInfo = templates.find((t) => t.id === template);

    if (templateInfo) {
      const created = await createProject(name, template, syntax, templateInfo.gitLink);

      if (created && resolverRef.current) {
        modalRef.current?.closeModal().catch((me: Error) => {
          console.error(me);
        });
        resolverRef.current(created);
        setName("");
        setTemplate("empty");
        setSyntax("mligo");
        setSyntaxes(pSyntaxes);
        setCreating(false);
        setTemplates(hardcodedTemplates);
      } else {
        setCreating(false);
      }
    } else {
      setCreating(false);
    }
  };

  const createProject = async (
    pName: string,
    pTemplate: string,
    pSyntax: string,
    gitLink?: string
  ) => {
    try {
      const Manager = ProjectManager;
      const created = await Manager.createProject(pName, pTemplate, pSyntax, gitLink);
      notification.success("Successful", `New project <b>${pName}</b> is created.`);
      return created;
    } catch (e) {
      if (e instanceof Error) {
        notification.error("Cannot Create the Project", e.message);
      } else {
        console.error(e);
      }
      return undefined;
    }
  };

  const renderTemplate = () => {
    return (
      <DropdownInput
        label="Template"
        options={templates.map((v) => {
          return { id: v.id, display: v.display };
        })}
        placeholder="(Please select a template)"
        value={template}
        onChange={(tmp: string) => {
          const templateInfo = templates.find((t) => t.id === tmp);
          if (templateInfo) {
            const newSyntaxes = pSyntaxes.filter((s) =>
              templateInfo.existingSyntaxes.includes(s.id)
            );
            if (newSyntaxes.length === 0) {
              newSyntaxes.push({ id: "mligo", display: "CameLIGO" });
            }
            setTemplate(tmp);
            setSyntaxes(newSyntaxes);
            setSyntax(newSyntaxes.map((s) => s.id).includes(syntax) ? syntax : newSyntaxes[0].id);
          }
        }}
        size="short"
      />
    );
  };

  return (
    <Modal
      ref={modalRef}
      title="Create a New Project"
      textConfirm="Create Project"
      onConfirm={onCreateProject}
      pending={creating && "Creating..."}
      confirmDisabled={!name || !!validName(name)}
    >
      <DebouncedFormGroup
        label="Project name"
        value={name}
        onChange={(n: string) => setName(n)}
        validator={validName}
      />
      {renderTemplate()}
      <DropdownInput
        label="Syntax"
        options={syntaxes}
        placeholder="(Please select a syntax)"
        value={syntax}
        onChange={(s: string) => setSyntax(s)}
      />
    </Modal>
  );
});

export default NewProjectModal;
