/****************************************************************************
**
** Copyright (C) 1992-$THISYEAR$ $TROLLTECH$. All rights reserved.
**
** This file is part of $PRODUCT$.
**
** $CPP_LICENSE$
**
** This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
** WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
**
****************************************************************************/

/*
  Copyright 2005 Roberto Raggi <roberto@kdevelop.org>

  Permission to use, copy, modify, distribute, and sell this software and its
  documentation for any purpose is hereby granted without fee, provided that
  the above copyright notice appear in all copies and that both that
  copyright notice and this permission notice appear in supporting
  documentation.

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
  KDEVELOP TEAM BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
  AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

#ifndef PP_MACRO_EXPANDER_H
#define PP_MACRO_EXPANDER_H

struct pp_frame
{
  pp_macro *expanding_macro;
  std::vector<std::string> *actuals;

  pp_frame (pp_macro *__expanding_macro, std::vector<std::string> *__actuals):
    expanding_macro (__expanding_macro), actuals (__actuals) {}
};

class pp_macro_expander
{
  pp_environment &env;
  pp_frame *frame;

  pp_skip_number skip_number;
  pp_skip_identifier skip_identifier;
  pp_skip_string_literal skip_string_literal;
  pp_skip_char_literal skip_char_literal;
  pp_skip_argument skip_argument;
  pp_skip_comment_or_divop skip_comment_or_divop;
  pp_skip_blanks skip_blanks;

  std::string const *resolve_formal (pp_fast_string const *__name)
  {
    assert (__name != 0);

    if (! frame)
      return 0;

    assert (frame->expanding_macro != 0);

    std::vector<pp_fast_string const *> const formals = frame->expanding_macro->formals;
    for (std::size_t index = 0; index < formals.size(); ++index)
      {
        pp_fast_string const *formal = formals[index];

        if (*formal != *__name)
          continue;
        else if (frame->actuals && index < frame->actuals->size())
          return &(*frame->actuals)[index];
        else
          assert (0); // internal error?
      }

    return 0;
  }

public: // attributes
  int lines;
  int generated_lines;

public:
  pp_macro_expander (pp_environment &__env, pp_frame *__frame = 0):
    env (__env), frame (__frame), lines (0), generated_lines (0) {}

  template <typename _InputIterator, typename _OutputIterator>
  _InputIterator operator () (_InputIterator __first, _InputIterator __last, _OutputIterator __result)
  {
    generated_lines = 0;
    __first = skip_blanks (__first, __last);
    lines = skip_blanks.lines;

    while (__first != __last)
      {
        if (*__first == '\n')
          {
            *__result++ = *__first;
            ++lines;

            __first = skip_blanks (++__first, __last);
            lines += skip_blanks.lines;

            if (__first != __last && *__first == '#')
              break;
          }
        else if (*__first == '#')
          {
            __first = skip_blanks (++__first, __last);
            lines += skip_blanks.lines;

            _InputIterator end_id = skip_identifier (__first, __last);
            *__result++ = '\"';
            int was = lines;
            this->operator () (__first, end_id, __result);
            lines += was;
            *__result++ = '\"';
            __first = end_id;
          }
        else if (*__first == '\"')
          {
            _InputIterator next_pos = skip_string_literal (__first, __last);
            lines += skip_string_literal.lines;
            std::copy (__first, next_pos, __result);
            __first = next_pos;
          }
        else if (*__first == '\'')
          {
            _InputIterator next_pos = skip_char_literal (__first, __last);
            lines += skip_char_literal.lines;
            std::copy (__first, next_pos, __result);
            __first = next_pos;
          }
        else if (_PP_internal::comment_p (__first, __last))
          {
            __first = skip_comment_or_divop (__first, __last);
            int n = skip_comment_or_divop.lines;
            lines += n;

            while (n-- > 0)
              *__result++ = '\n';
          }
        else if (pp_isspace (*__first))
          {
            for (; __first != __last; ++__first)
              {
                if (*__first == '\n' || !pp_isspace (*__first))
                  break;
              }

            *__result = ' ';
          }
        else if (pp_isdigit (*__first))
          {
            _InputIterator next_pos = skip_number (__first, __last);
            lines += skip_number.lines;
            std::copy (__first, next_pos, __result);
            __first = next_pos;
          }
        else if (pp_isalpha (*__first) || *__first == '_')
          {
            _InputIterator name_begin = __first;
            _InputIterator name_end = skip_identifier (__first, __last);
            __first = name_end; // advance

            // search for the paste token
            _InputIterator next = skip_blanks (__first, __last);
            if (next != __last && *next == '#')
              {
                ++next;
                if (next != __last && *next == '#')
                  __first = skip_blanks(++next, __last);
              }

            std::ptrdiff_t name_size = std::distance (name_begin, name_end);
            assert (name_size >= 0 && name_size < 512);

            char name_buffer[512], *cp = name_buffer;
            std::copy (name_begin, name_end, cp);
            name_buffer[name_end - name_begin] = '\0';

            pp_fast_string fast_name (name_buffer, name_size);

            if (std::string const *actual = resolve_formal (&fast_name))
              {
                std::copy (actual->begin (), actual->end (), __result);
                continue;
              }

            static bool hide_next = false; // ### remove me

            pp_macro *macro = env.resolve (name_buffer, name_size);
            if (! macro || macro->hidden || hide_next)
              {
                hide_next = !strcmp (name_buffer, "defined");
                std::copy (name_begin, name_end, __result);
                continue;
              }

            if (! macro->function_like)
              {
                pp_macro_expander expand_macro (env);
                macro->hidden = true;
                expand_macro (macro->definition->begin (), macro->definition->end (), __result);
                macro->hidden = false;
                generated_lines += expand_macro.lines;
                continue;
              }

            // function like macro
            _InputIterator arg_it = __first;

            if (arg_it == __last || *arg_it  != '(')
              {
                std::copy (name_begin, name_end, __result);
                __first = name_end;
                continue;
              }

            std::vector<std::string> actuals;
            actuals.reserve (5);
            ++arg_it; // skip '('

            pp_macro_expander expand_actual (env, frame);

            _InputIterator arg_end = skip_argument_variadics (actuals, macro, arg_it, __last);
            if (arg_it != arg_end)
              {
                std::string actual (arg_it, arg_end);
                actuals.resize (actuals.size() + 1);
                actuals.back ().reserve (255);
                expand_actual (actual.begin (), actual.end(), std::back_inserter (actuals.back()));
                arg_it = arg_end;
              }

            while (arg_it != __last && *arg_end == ',')
              {
                ++arg_it; // skip ','

                arg_end = skip_argument_variadics (actuals, macro, arg_it, __last);
                std::string actual (arg_it, arg_end);
                actuals.resize (actuals.size() + 1);
                actuals.back ().reserve (255);
                expand_actual (actual.begin (), actual.end(), std::back_inserter (actuals.back()));
                arg_it = arg_end;
              }

              assert (arg_it != __last && *arg_it == ')');

              ++arg_it; // skip ')'
              __first = arg_it;

#if 0 // ### enable me
              assert ((macro->variadics && macro->formals.size () >= actuals.size ())
                          || macro->formals.size() == actuals.size());
#endif

              pp_frame frame (macro, &actuals);
              pp_macro_expander expand_macro (env, &frame);
              macro->hidden = true;
              expand_macro (macro->definition->begin (), macro->definition->end (), __result);
              macro->hidden = false;
              generated_lines += expand_macro.lines;
          }
        else
          *__result++ = *__first++;
      }

    return __first;
  }

  template <typename _InputIterator>
  _InputIterator skip_argument_variadics (std::vector<std::string> const &__actuals, pp_macro *__macro,
                                          _InputIterator __first, _InputIterator __last)
  {
    _InputIterator arg_end = skip_argument (__first, __last);

    while (__macro->variadics && __first != arg_end && arg_end != __last && *arg_end == ','
        && (__actuals.size () + 1) == __macro->formals.size ())
      {
        arg_end = skip_argument (++arg_end, __last);
      }

    return arg_end;
  }
};

#endif // PP_MACRO_EXPANDER_H

// kate: indent-width 2;
