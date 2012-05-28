(defpackage :cl-triangulation
  (:use :cl))
(in-package :cl-triangulation)

(defun lines-intersect-p (l1x1 l1y1 l1x2 l1y2 l2x1 l2y1 l2x2 l2y2)
  "Detects if two lines intersect."
  (format t "testing intersect: ~a ~a~%" (list (list l1x1 l1y1) (list l1x2 l1y2))
                                         (list (list l2x1 l2y1) (list l2x2 l2y2)))
  (let ((l1sx (- l1x2 l1x1))
        (l1sy (- l1y2 l1y1))
        (l2sx (- l2x2 l2x1))
        (l2sy (- l2y2 l2y1)))
    (let ((div (+ (* (- l2sx) l1sy) (* l1sx l2sy))))
      (when (zerop div)
        (return-from lines-intersect-p nil))
      (let ((ls (/ (+ (* (- l1sy) (- l1x1 l2x1))
                      (* l1sx (- l1y1 l2y1)))
                   div))
            (lt (/ (- (* l2sx (- l1y1 l2y1))
                      (* l2sy (- l1x1 l2x1)))
                   div)))
        (format t "ls, lt: ~a, ~a~%" ls lt)
        (and (< 0 ls 1)
             (< 0 lt 1))))))

;; int pnpoly(int nvert, float *vertx, float *verty, float testx, float testy)
;; {
;;   int i, j, c = 0;
;;   for (i = 0, j = nvert-1; i < nvert; j = i++) {
;;     if ( ((verty[i]>testy) != (verty[j]>testy)) &&
;; 	 (testx < (vertx[j]-vertx[i]) * (testy-verty[i]) / (verty[j]-verty[i]) + vertx[i]) )
;;        c = !c;
;;   }
;;   return c;
;; }

(defun point-in-polygon-p (point polygon-points)
  (let ((x (car point))
        (y (cadr point))
        (j (1- (length polygon-points))))
    (dotimes (i (length polygon-points))

      (setf j i))

(defun line-inside-polygon-p (line-points polygon-points)
  (let ((lx1 (car line-points))
        (ly1 (cadr line-points))
        (lx2 (caddr line-points))
        (ly2 (cadddr line-points)))
    (dotimes (i (length polygon-points))
      (let ((cur-point (aref polygon-points i))
            (next-point (aref polygon-points (if (<= (length polygon-points) (1+ i)) 0 (1+ i)))))
        (unless (and (eq lx1 (car cur-point))
                     (eq ly1 (cadr cur-point))
                     (eq lx2 (car next-point))
                     (eq ly2 (cadr next-point)))
          (when (lines-intersect-p lx1 ly1 lx2 ly2
                                   (car cur-point) (cadr cur-point) (car next-point) (cadr next-point))
            (format t "Got intersection in poly: ~a ~a~%" (list (list lx1 ly1) (list lx2 ly2)) (list cur-point next-point))
            (return-from line-inside-polygon-p nil))))))
  t)

(defun triangulate (points)
  (assert (vectorp points))
  (let ((triangles nil))
    (loop for i from 0 do
      (when (>= i (length points))
        (return))
      (when (eq (length points) 3)
        (let ((p1 (aref points 0))
              (p2 (aref points 1))
              (p3 (aref points 2)))
          (push (list (list (car p1) (cadr p1))
                      (list (car p2) (cadr p2))
                      (list (car p3) (cadr p3)))
                triangles))
        (return))
      (let ((cur-point (aref points i))
            (next-point (aref points (if (<= (length points) (1+ i)) 0 (1+ i))))
            (prev-point (aref points (if (zerop i) (1- (length points)) (1- i)))))
        (loop for point across points do
          (unless (or (equal point cur-point)
                      (equal point next-point)
                      (equal point prev-point))
            ;(format t "Made it: ~a ~a~%" cur-point point)
            (if (line-inside-polygon-p (list (car cur-point) (cadr cur-point) (car point) (cadr point)) points)
                (progn
                  ;(format t "Line fits in ply!~%")
                  (push (list (list (car cur-point) (cadr cur-point))
                              (list (car next-point) (cadr next-point))
                              (list (car point) (cadr point)))
                        triangles)
                  (setf points (remove-if (lambda (p) (equal p next-point)) points))
                  (setf next-point point))
                (return))))))
    triangles))

(defparameter *points* #( (0 0) (4 0) (4 4) (0 4) (0 3) (2 3) (1 3) (0 1) ))
(triangulate *points*)
