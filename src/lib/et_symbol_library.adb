------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     SCHEMATIC SYMBOL LIBRARY                             --
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.text_io;				use ada.text_io;

-- with ada.exceptions; 			use ada.exceptions;


package body et_symbol_library is
	


	function get_symbol_model_name (
		symbol_cursor : in pac_symbol_models.cursor)
		return pac_symbol_model_name.bounded_string
	is begin
		return key (symbol_cursor);
	end;
	

	function get_symbol_model_name (
		symbol_cursor : in pac_symbol_models.cursor)
		return string
	is begin
		return pac_symbol_model_name.to_string (key (symbol_cursor));
	end;



	
	procedure create_symbol (
		symbol_name		: in pac_symbol_model_name.bounded_string; -- libraries/symbols/nand.sym
		appearance		: in type_appearance;
		log_threshold	: in type_log_level) 
	is begin
		log (text => "creating symbol " & to_string (symbol_name) & " ...", level => log_threshold);
		log_indentation_up;
		log (text => "appearance " & to_string (appearance) & " ...", level => log_threshold);
		
		-- Test if symbol already exists. If already exists, issue warning and exit.
		if contains (symbol_library, symbol_name) then
			log (WARNING, text => "symbol already exists -> skipped", level => log_threshold + 1);
		else
			case appearance is
				when APPEARANCE_PCB =>
					insert (
						container	=> symbol_library,
						key			=> symbol_name,
						new_item	=> (appearance => APPEARANCE_PCB, others => <>)
						);

				when APPEARANCE_VIRTUAL =>
					insert (
						container	=> symbol_library,
						key			=> symbol_name,
						new_item	=> (appearance => APPEARANCE_VIRTUAL, others => <>)
						);
			end case;					
		end if;

		log_indentation_down;
	end create_symbol;


	

	
	procedure get_symbol_model (
		model_file	: in pac_symbol_model_name.bounded_string;
		cursor		: in out pac_symbol_models.cursor)
	is begin
		cursor := symbol_library.find (model_file);
	end;

	

	function get_symbol_model (
		model_name : in pac_symbol_model_name.bounded_string)
		return pac_symbol_models.cursor
	is begin
		return symbol_library.find (model_name);
	end;

	
	
	
	function is_real (
		symbol : in pac_symbol_models.cursor)
		return boolean
	is begin
		case element (symbol).appearance is
			when APPEARANCE_PCB		=> return true;
			when APPEARANCE_VIRTUAL	=> return false;
		end case;
	end is_real;



	function get_port_positions (
		symbol	: in pac_symbol_models.cursor)
		return pac_points.list
	is begin
		return get_port_positions (element (symbol));
	end get_port_positions;




	function get_placeholders (
		symbol : in pac_symbol_models.cursor)
		return type_text_placeholders
	is 
		sym : type_symbol_model renames element (symbol);
	begin
		if is_real (sym) then
			return sym.placeholders;
		else
			return (others => <>);
		end if;
	end;



	
	function get_default_placeholders (
		symbol_cursor	: in pac_symbol_models.cursor;
		destination		: in type_object_position)
		return type_text_placeholders
	is
		sym : type_symbol_model renames element (symbol_cursor);
		
		r : type_text_placeholders; -- to be returned
	begin
		r.name		:= sym.placeholders.name;
		r.value		:= sym.placeholders.value;
		r.purpose	:= sym.placeholders.purpose;

		-- Rotate the positions of placeholders 
		-- according to rotation given by destination:
		rotate_placeholders (r, get_rotation (destination));
		
		return r;
	end get_default_placeholders;


	


	function get_symbol (
		symbol	: in pac_symbol_models.cursor)
		return type_symbol_model
	is begin
		return element (symbol);
	end;



	
	
end et_symbol_library;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
