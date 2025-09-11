#!/usr/bin/env python3
import os
import json
import argparse
from pathlib import Path

# Optional .env support
try:
    from dotenv import load_dotenv
    load_dotenv(dotenv_path=".env")
except Exception:
    pass

from osfclient import OSF

def load_manifest(path):
    with open(path, "r", encoding="utf-8") as f:
        return json.load(f)

def main():
    parser = argparse.ArgumentParser(description="Fetch data files from OSF per a manifest.")
    parser.add_argument("--manifest", required=True, help="Path to osf_manifest.json")
    parser.add_argument("--project", default="", help="OSF project id (overrides manifest)")
    parser.add_argument("--component", default="", help="OSF component id (optional; overrides manifest)")
    parser.add_argument("--token", default="", help="OSF token (uses env OSF_TOKEN if empty)")
    args = parser.parse_args()

    manifest = load_manifest(args.manifest)
    project_id = args.project or manifest.get("project") or os.environ.get("OSF_PROJECT")
    component_id = args.component or manifest.get("component") or os.environ.get("OSF_COMPONENT", "")
    token = args.token or os.environ.get("OSF_TOKEN")

    if not project_id:
        raise SystemExit("OSF project id missing. Set in manifest, --project, or OSF_PROJECT env.")
    if not token:
        raise SystemExit("OSF token missing. Provide via --token or OSF_TOKEN env.")

    osf = OSF(token=token)

    # Use OSF.project() for both projects and components
    try:
        node = osf.project(component_id) if component_id else osf.project(project_id)
    except Exception as e:
        raise SystemExit(f"Failed to access OSF node. Check IDs and token. Details: {e}")

    store = node.storage("osfstorage")

    # Handle both osfclient APIs: files as attribute (generator) or method
    files_attr = getattr(store, "files", None)
    if files_attr is None:
        raise SystemExit("osfclient: storage has no 'files' attribute/method.")
    files_iter = files_attr() if callable(files_attr) else files_attr

    files = manifest.get("files", [])
    if not files:
        print("[INFO] No files listed in manifest; nothing to download yet.")
        return

    for entry in files:
        remote = entry["osf_path"].lstrip("/")
        dest = Path(entry["dest"])
        dest.parent.mkdir(parents=True, exist_ok=True)
        # lazily fetch file handles by path
        # osfclient doesn't provide direct dict lookup; iterate to find a match
        match = None
        for f in files_iter:
            if f.path.lstrip("/") == remote:
                match = f
                break
        if not match:
            print(f"[WARN] Could not find remote path on OSF: {remote}")
            # reset iterator (osfclient generators can't be rewound; rebuild for next loop)
            files_attr = getattr(store, "files", None)
            files_iter = files_attr() if callable(files_attr) else files_attr
            continue

        print(f"[INFO] Downloading {remote} -> {dest}")
        with open(dest, "wb") as fp:
            match.write_to(fp)

        # reset iterator for next lookup
        files_attr = getattr(store, "files", None)
        files_iter = files_attr() if callable(files_attr) else files_attr

    print("[INFO] Done.")

if __name__ == "__main__":
    main()
