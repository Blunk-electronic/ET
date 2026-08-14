------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          KICAD V6 / S-EXPRESSIONS                        --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab width in your editor to 4.

with ada.text_io;
with ada.strings.unbounded;		use ada.strings.unbounded;
with ada.characters.latin_1;

package body et_kicad_v6.sexp is


	------------------------------------------------------------------
	-- TOKENIZER
	------------------------------------------------------------------

	type type_token_kind is (TOK_LPAREN, TOK_RPAREN, TOK_ATOM, TOK_EOF);

	type type_token is record
		kind	: type_token_kind;
		text	: unbounded_string; -- meaningful for TOK_ATOM only: interpreted content
		quoted	: boolean := false;
		line	: positive := 1;
	end record;

	package pac_tokens is new ada.containers.vectors (positive, type_token);


	function is_delimiter (c : in character) return boolean is
	begin
		return c = ' ' or c = ada.characters.latin_1.ht
			or c = ada.characters.latin_1.cr or c = ada.characters.latin_1.lf
			or c = '(' or c = ')' or c = '"';
	end is_delimiter;


	-- Splits the given source text into a flat token stream. This is
	-- the only place that reasons about individual characters --
	-- everything above it works on tokens or nodes:
	function tokenize (source : in string) return pac_tokens.vector is
		use ada.characters.latin_1;

		tokens	: pac_tokens.vector;
		i		: natural := source'first;
		line	: positive := 1;
	begin
		while i <= source'last loop
			declare
				c : constant character := source (i);
			begin
				case c is

					when ' ' | ht | cr =>
						i := i + 1;

					when lf =>
						line := line + 1;
						i := i + 1;

					when '(' =>
						tokens.append ((kind => TOK_LPAREN, text => null_unbounded_string, quoted => false, line => line));
						i := i + 1;

					when ')' =>
						tokens.append ((kind => TOK_RPAREN, text => null_unbounded_string, quoted => false, line => line));
						i := i + 1;

					when '"' =>
						declare
							start_line	: constant positive := line;
							buffer		: unbounded_string;
							terminated	: boolean := false;
						begin
							i := i + 1; -- skip opening quote

							while i <= source'last loop
								declare
									cc : constant character := source (i);
								begin
									if cc = '"' then
										i := i + 1;
										terminated := true;
										exit;

									elsif cc = '\' and then i < source'last then
										declare
											esc : constant character := source (i + 1);
										begin
											case esc is
												when '"' => append (buffer, '"');  i := i + 2;
												when '\' => append (buffer, '\');  i := i + 2;
												when 'n' => append (buffer, lf);   i := i + 2;
												when others =>
													-- Unknown escape: keep it literally rather
													-- than silently dropping the backslash --
													-- CS: no such escape observed in practice.
													append (buffer, '\');
													append (buffer, esc);
													i := i + 2;
											end case;
										end;

									else
										-- A raw newline inside a quoted atom is not expected
										-- (see package description), but is tolerated rather
										-- than mis-parsed, keeping line numbering in sync:
										if cc = lf then
											line := line + 1;
										end if;

										append (buffer, cc);
										i := i + 1;
									end if;
								end;
							end loop;

							if not terminated then
								raise sexp_syntax_error with
									"unterminated quoted string starting at line" & positive'image (start_line);
							end if;

							tokens.append ((kind => TOK_ATOM, text => buffer, quoted => true, line => start_line));
						end;

					when others =>
						declare
							start_pos	: constant positive := i;
							start_line	: constant positive := line;
						begin
							while i <= source'last and then not is_delimiter (source (i)) loop
								i := i + 1;
							end loop;

							tokens.append ((
								kind	=> TOK_ATOM,
								text	=> to_unbounded_string (source (start_pos .. i - 1)),
								quoted	=> false,
								line	=> start_line));
						end;
				end case;
			end;
		end loop;

		tokens.append ((kind => TOK_EOF, text => null_unbounded_string, quoted => false, line => line));
		return tokens;
	end tokenize;


	------------------------------------------------------------------
	-- PARSER
	------------------------------------------------------------------

	-- Parses exactly one expression starting at tokens (pos), and
	-- advances pos to just past it:
	function parse_one (tokens : in pac_tokens.vector; pos : in out positive) return type_node_access is
		t : constant type_token := tokens (pos);
	begin
		case t.kind is

			when TOK_LPAREN =>
				pos := pos + 1;

				declare
					children : pac_node_children.vector;
				begin
					loop
						if pos > natural (tokens.length) then
							raise sexp_syntax_error with
								"unbalanced parentheses: missing ')' for list opened at line"
								& positive'image (t.line);
						end if;

						exit when tokens (pos).kind = TOK_RPAREN;

						children.append (parse_one (tokens, pos));
					end loop;

					pos := pos + 1; -- consume the ')'

					return new type_node'(kind => SEXP_LIST, children => children);
				end;

			when TOK_ATOM =>
				pos := pos + 1;
				return new type_node'(kind => SEXP_ATOM, text => t.text, quoted => t.quoted, line => t.line);

			when TOK_RPAREN =>
				raise sexp_syntax_error with "unexpected ')' at line" & positive'image (t.line);

			when TOK_EOF =>
				raise sexp_syntax_error with "unexpected end of input";
		end case;
	end parse_one;


	function parse_string (source : in string) return type_node is
		tokens	: constant pac_tokens.vector := tokenize (source);
		pos		: positive := tokens.first_index;
		root	: type_node_access;
	begin
		if tokens (pos).kind = TOK_EOF then
			raise sexp_syntax_error with "empty input";
		end if;

		root := parse_one (tokens, pos);

		if tokens (pos).kind /= TOK_EOF then
			raise sexp_syntax_error with
				"trailing content after top-level expression, at line" & positive'image (tokens (pos).line);
		end if;

		return root.all;
	end parse_string;


	function parse_file (file_name : in string) return type_node is
		use ada.text_io;
		file	: file_type;
		buffer	: unbounded_string;
	begin
		open (file, in_file, file_name);

		while not end_of_file (file) loop
			append (buffer, get_line (file));
			append (buffer, ada.characters.latin_1.lf);
		end loop;

		close (file);

		return parse_string (to_string (buffer));
	end parse_file;


	------------------------------------------------------------------
	-- TREE HELPERS
	------------------------------------------------------------------

	function kind (node : in type_node) return type_node_kind is
	begin
		return node.kind;
	end kind;


	function child_count (node : in type_node) return natural is
	begin
		if node.kind = SEXP_LIST then
			return natural (node.children.length);
		else
			return 0;
		end if;
	end child_count;


	function get_child (node : in type_node; index : in positive) return type_node is
	begin
		return node.children (index).all;
	end get_child;


	function get_child_access (node : in type_node; index : in positive) return type_node_access is
	begin
		return node.children (index);
	end get_child_access;


	function head (node : in type_node) return string is
	begin
		if node.kind = SEXP_LIST and then natural (node.children.length) > 0 then
			declare
				first_child : constant type_node_access := node.children.first_element;
			begin
				if first_child.kind = SEXP_ATOM and then not first_child.quoted then
					return to_string (first_child.text);
				end if;
			end;
		end if;

		return "";
	end head;


	function atom_text (node : in type_node) return string is
	begin
		return to_string (node.text);
	end atom_text;


	function atom_was_quoted (node : in type_node) return boolean is
	begin
		return node.quoted;
	end atom_was_quoted;


	function atom_line (node : in type_node) return positive is
	begin
		return node.line;
	end atom_line;


	function atom_to_real (node : in type_node) return long_float is
	begin
		return long_float'value (atom_text (node));
	exception
		when constraint_error =>
			if node.kind /= SEXP_ATOM then
				raise;
			end if;

			raise sexp_syntax_error with
				"expected a number at line" & positive'image (node.line)
				& ", got '" & atom_text (node) & "'";
	end atom_to_real;


	function atom_to_natural (node : in type_node) return natural is
	begin
		return natural'value (atom_text (node));
	exception
		when constraint_error =>
			if node.kind /= SEXP_ATOM then
				raise;
			end if;

			raise sexp_syntax_error with
				"expected a natural number at line" & positive'image (node.line)
				& ", got '" & atom_text (node) & "'";
	end atom_to_natural;


	function atom_to_integer (node : in type_node) return integer is
	begin
		return integer'value (atom_text (node));
	exception
		when constraint_error =>
			if node.kind /= SEXP_ATOM then
				raise;
			end if;

			raise sexp_syntax_error with
				"expected an integer at line" & positive'image (node.line)
				& ", got '" & atom_text (node) & "'";
	end atom_to_integer;


	function atom_to_yes_no (node : in type_node) return boolean is
		text : constant string := atom_text (node); -- Constraint_Error propagates if node is not an atom
	begin
		if text = "yes" then
			return true;
		elsif text = "no" then
			return false;
		else
			raise sexp_syntax_error with
				"expected 'yes' or 'no' at line" & positive'image (node.line)
				& ", got '" & text & "'";
		end if;
	end atom_to_yes_no;


	function find_first_child (node : in type_node; tag : in string) return type_node is
		c : pac_node_children.cursor;
	begin
		if node.kind /= SEXP_LIST then
			return none;
		end if;

		c := node.children.first;

		while pac_node_children.has_element (c) loop
			if head (pac_node_children.element (c).all) = tag then
				return pac_node_children.element (c).all;
			end if;

			pac_node_children.next (c);
		end loop;

		return none;
	end find_first_child;


	function find_all_children (node : in type_node; tag : in string) return pac_node_list.vector is
		result	: pac_node_list.vector;
		c		: pac_node_children.cursor;
	begin
		if node.kind /= SEXP_LIST then
			return result;
		end if;

		c := node.children.first;

		while pac_node_children.has_element (c) loop
			if head (pac_node_children.element (c).all) = tag then
				result.append (pac_node_children.element (c).all);
			end if;

			pac_node_children.next (c);
		end loop;

		return result;
	end find_all_children;


end et_kicad_v6.sexp;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
