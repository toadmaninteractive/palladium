import { Inject, Injectable } from '@angular/core';
import { DOCUMENT } from '@angular/common';

@Injectable({
    providedIn: 'root',
})
export class ClipboardService {
    private dom: Document;

    // CAUTION: This service is tightly couped to the browser DOM (Document Object Model).
    // But, by injecting the "document" reference rather than trying to reference it globally, we can at least pretend that we are trying
    // to lower the tight coupling.
    constructor(@Inject(DOCUMENT) dom: Document) {
        this.dom = dom;
    }

    // Copy the given value to the user system clipboard. Returns a promise that  resolves to the given value on success or rejects with
    // the raised Error.
    public copy(value: string, isHtml: boolean = false): Promise<any> {
        return new Promise(
            (resolve, reject): void => {
                let textarea = null;
                try {
                    // In order to execute the "Copy" command, we actually have to have a "selection" in the currently rendered document.
                    // As such, we're going to inject a Textarea element and .select() it in order to force a selection.
                    textarea = this.dom.createElement('textarea'); // Textarea is being rendered off-screen.
                    textarea.style.height = '0px';
                    textarea.style.left = '-100px';
                    textarea.style.opacity = '0';
                    textarea.style.position = 'fixed';
                    textarea.style.top = '-100px';
                    textarea.style.width = '0px';
                    this.dom.body.appendChild(textarea);

                    // Set and select the value (creating an active Selection range).
                    textarea.value = value;
                    textarea.select();

                    // Ask the browser to copy the current selection to the clipboard.
                    this.dom.execCommand('copy');
                    resolve(value);
                } finally {
                    // Cleanup - remove the Textarea from the DOM if it was injected.
                    if (textarea && textarea.parentNode) {
                        textarea.parentNode.removeChild(textarea);
                    }
                }
            }
        );
    }
}
