;;; -*- mode: lisp; package: ansify -*-
;;;
;;; ansify/config.l
;;;
;;; Author:    bowbow99  <bowbow99@gmail.com>
;;; License:   MIT (See COPYING.mit)
;;;
;;; This file is part of xyzzy extension "ansify".

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify/base"))

(in-package :ansify.ext)


;;;; for typespec+

(export '(*warn-on-undefined-typespec*
          *warn-on-invalid-typespec*
          ))

(defvar *warn-on-undefined-typespec* nil
  "*����`�̌^�w��q���g��ꂽ��x�����邩�ǂ����B

`typep`, `typecase`, `check-type` �ȂǁA�I�u�W�F�N�g���^�w��q�̌^����
�������ׂ�I�y���[�^�̓���ɉe������B")

(defvar *warn-on-invalid-typespec* nil
  "*�s���Ȍ^�w��q���g��ꂽ��x�����邩�ǂ����B

�s���Ȍ^�w��q�ƌ��Ȃ����͈̂ȉ��̏ꍇ:
- `and` �� `not` �Ȃǂ̃��X�g�łȂ���΂Ȃ�Ȃ��^�w��q���V���{���݂̂�
  �g�p����
- `satisfies` �� predicate �֐����V���{���ȊO�Ŏw�肵��

������� ANSI �ł͋֎~����Ă��邪�Axyzzy �W���� `lisp:typep` �ł̓G���[
�Ȃǂɂ͂Ȃ�Ȃ��B
���X�g�łȂ���΂Ȃ�Ȃ��^�w��q�́A�����Ȃ��̃��X�g�`�����������̂Ƃ���
������B�i�������K�v�ȏꍇ�̓G���[�ɂȂ�B�j
`satisfies` �ł� lambda ���� `#'some-function` �̂悤�Ȍ`���ł����̊֐�
���g���B�֐��łȂ������ꍇ�̓G���[�ɂȂ�B
")


;;;; for typecase.l

(export '(*warn-on-otherwise-in-middle*
          ))

(defvar *warn-on-otherwise-in-middle* nil
  "*case ���̓r���� otherwise �߂��������ꍇ�Ɍx�����邩�ǂ����B

otherwise �߂Ă̂͐߂̃L�[�� `t` �� `otherwise` �ɂȂ��Ă���߁BANSI ��
�����ɉ��߂���Ƃ����r���ɒu���Ă͂����Ȃ��B

case ���̓r���ł����Ă� otherwise �߂�����΂����ŕ���͏I�����A�ȍ~�̐�
�͖��������B")


;;;; for function.l

(export '(*warn-on-redefine-function*
          ))

(defvar *warn-on-redefine-function* nil
  "*�֐���}�N�����Ē�`������x�����邩�ǂ����B")

(provide "ansify/config")
;;; ansify/config.l ends here.