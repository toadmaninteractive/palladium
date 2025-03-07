import { Injectable } from '@angular/core';

@Injectable({
    providedIn: 'root',
})
export class CanvasService {
    resize(canvas: any, maxWidth: number, maxHeight: number) {
        const imgWidth = canvas.width,
            imgHeight = canvas.height,
            ratio1 = maxWidth / imgWidth,
            ratio2 = maxHeight / imgHeight,
            ratio = ratio1 < ratio2 ? ratio1 : ratio2;

        // Get canvas context and create canvas copy
        const canvasContext = canvas.getContext('2d'),
            canvasCopy = document.createElement('canvas'),
            copyContext = canvasCopy.getContext('2d'),
            canvasCopy2 = document.createElement('canvas'),
            copyContext2 = canvasCopy2.getContext('2d');

        canvasCopy.width = imgWidth;
        canvasCopy.height = imgHeight;
        copyContext.drawImage(canvas, 0, 0);

        // Init
        canvasCopy2.width = imgWidth;
        canvasCopy2.height = imgHeight;
        copyContext2.drawImage(canvasCopy, 0, 0, canvasCopy.width, canvasCopy.height, 0, 0, canvasCopy2.width, canvasCopy2.height);

        const rounds = 2,
            roundRatio = ratio * rounds;

        for (let i = 1; i <= rounds; i++) {
            // Tmp
            canvasCopy.width = imgWidth * roundRatio / i;
            canvasCopy.height = imgHeight * roundRatio / i;
            copyContext.drawImage(canvasCopy2, 0, 0, canvasCopy2.width, canvasCopy2.height, 0, 0, canvasCopy.width, canvasCopy.height);

            // Copy back
            canvasCopy2.width = imgWidth * roundRatio / i;
            canvasCopy2.height = imgHeight * roundRatio / i;
            copyContext2.drawImage(canvasCopy, 0, 0, canvasCopy.width, canvasCopy.height, 0, 0, canvasCopy2.width, canvasCopy2.height);
        }

        // Copy back to canvas
        canvas.width = imgWidth * roundRatio / rounds;
        canvas.height = imgHeight * roundRatio / rounds;
        canvasContext.drawImage(canvasCopy2, 0, 0, canvasCopy2.width, canvasCopy2.height, 0, 0, canvas.width, canvas.height);
    }
}
