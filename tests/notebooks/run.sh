#!/usr/bin/env bash
#
# Run all test notebooks (test-*.ipynb) found under the notebook directory.
#
# Notebooks are executed with `jupyter nbconvert` inside a temporary copy of the
# test tree so the relative layout between notebooks/ and data/ is preserved
# (notebooks reference their data as ../data/...). Output is captured and only
# shown when a notebook fails. Execution is sequential and fails fast; the
# temporary directory is cleaned up only when every notebook passes.

set -euo pipefail

# --- Layout ------------------------------------------------------------------
# This script lives in the notebook directory; data/ is its sibling.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
NOTEBOOK_DIR="${SCRIPT_DIR}"
BASE_DIR="$(dirname "${NOTEBOOK_DIR}")"        # holds notebooks/ and data/
NOTEBOOK_REL="$(basename "${NOTEBOOK_DIR}")"   # e.g. "notebooks"

# --- Presentation ------------------------------------------------------------
if [[ -t 1 ]]; then
    C_RED=$'\033[1;41m'      # bold, red background
    C_GREEN=$'\033[1;42m'    # bold, green background
    C_RESET=$'\033[0m'
else
    C_RED=""
    C_GREEN=""
    C_RESET=""
fi
readonly REPORT_WIDTH=60

# --- Helpers -----------------------------------------------------------------

# Count code cells in a notebook.
count_code_cells() {
    local notebook="$1"
    grep -c '"cell_type": "code"' "${notebook}" || true
}

# Print the leading label "<name> (<n> cells)", padded, without a newline.
report_start() {
    local name="$1" cells="$2"
    local left
    left="$(printf '%s (%s cells)' "${name}" "${cells}")"
    printf '%s%*s' "${left}" "$((REPORT_WIDTH - ${#left}))" ""
}

# Finish the current line with the colored "<STATUS>".
report_status() {
    local status="$1" color="$2"
    printf '%s %s %s\n' "${color}" "${status}" "${C_RESET}"
}

# Locate every test notebook (recursively), excluding checkpoint copies.
find_notebooks() {
    find "${NOTEBOOK_DIR}" \
        -type d -name '.ipynb_checkpoints' -prune -o \
        -type f -name 'test-*.ipynb' -print | sort
}

# Copy the test tree into a fresh temp dir, preserving notebooks/ + data/.
setup_workdir() {
    local workdir
    workdir="$(mktemp -d)"
    cp -R "${BASE_DIR}/." "${workdir}/"
    printf '%s' "${workdir}"
}

# Execute a single notebook with nbconvert; all output goes to the log file.
run_notebook() {
    local notebook="$1" logfile="$2"
    jupyter nbconvert \
        --to notebook \
        --execute \
        --ExecutePreprocessor.store_widget_state=False \
        --output "$(basename "${notebook%.ipynb}")-output.ipynb" \
        "${notebook}" \
        >"${logfile}" 2>&1
}

# --- Main --------------------------------------------------------------------

main() {
    local workdir
    workdir="$(setup_workdir)"

    local notebooks=()
    local nb
    while IFS= read -r nb; do
        notebooks+=("${nb}")
    done < <(find_notebooks)

    if [[ ${#notebooks[@]} -eq 0 ]]; then
        echo "No test notebooks found under ${NOTEBOOK_DIR}" >&2
        rm -rf "${workdir}"
        exit 1
    fi

    local failed=0
    for nb in "${notebooks[@]}"; do
        local name cells rel work_nb logfile
        name="$(basename "${nb%.ipynb}")"
        cells="$(count_code_cells "${nb}")"

        # Path of this notebook inside the copied tree.
        rel="${nb#"${NOTEBOOK_DIR}/"}"
        work_nb="${workdir}/${NOTEBOOK_REL}/${rel}"
        logfile="$(mktemp)"

        report_start "${name}" "${cells}"
        if run_notebook "${work_nb}" "${logfile}"; then
            report_status "PASS" "${C_GREEN}"
            rm -f "${logfile}"
        else
            report_status "FAIL" "${C_RED}"
            echo
            cat "${logfile}"
            rm -f "${logfile}"
            failed=1
            break  # fail fast
        fi
    done

    if [[ ${failed} -ne 0 ]]; then
        echo >&2
        echo "Workdir kept for inspection: ${workdir}" >&2
        exit 1
    fi

    rm -rf "${workdir}"
}

main "$@"
