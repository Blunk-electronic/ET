------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              PCB STACK                                   --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
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
with ada.strings;				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.exceptions;



package body et_pcb_stack is



	function layer_stacks_equally (
		right, left : in pac_signal_layers.set)
		return boolean
	is
		use pac_signal_layers;
	begin
		if right = left then
			return true;
		else
			return false;
		end if;
	end layer_stacks_equally;


	

	function layer_stack_contains (
		stack		: pac_signal_layers.set;
		layer		: type_signal_layer;
		exclusively	: in boolean := false)
		return boolean
	is
		result : boolean := false;
	begin
		case exclusively is
			when FALSE =>
				if stack.contains (layer) then
					result := true;
				end if;

			when TRUE =>
				if stack.length = 1 and stack.contains (layer) then
					result := true;
				end if;
		end case;

		return result;
	end layer_stack_contains;
	

	
	
	
	
	
	function get_deepest_layer (
		stack : in type_stack) 
		return type_signal_layer
	is begin
		-- Because the bottom layer is always there, we add 1:
		return stack.layers.last_index + 1;
	end get_deepest_layer;

	

	
	
	function signal_layer_valid (
		signal_layer 	: in type_signal_layer;
		check_layers	: in et_pcb_stack.type_layer_check)
		return boolean 
	is 
		result : boolean := false;
	begin
		if check_layers.check = YES then
			if signal_layer <= check_layers.deepest_layer then
				result := true;
			else
				result := false;
			end if;
		else 
			result := true; -- no layer check requested
		end if;

		return result;
	end signal_layer_valid;	



	


	procedure signal_layer_invalid (
		line			: in type_fields_of_line;
		signal_layer	: in type_signal_layer;
		check_layers	: in et_pcb_stack.type_layer_check) 
	is begin
		--log (WARNING, affected_line (line) & "Signal layer " & to_string (signal_layer) &
			 --" is deeper than the deepest signal layer " &
			 --to_string (check_layers.deepest_layer) & " !" &
		--" Objects in this layer will be ignored !");
		
		-- CS raise semantic_error_1 with
		raise constraint_error with
			"ERROR: " & get_affected_line (line) 
			& "Signal layer " & to_string (signal_layer) 
			& " is deeper than the deepest signal layer " 
			& to_string (check_layers.deepest_layer) & " !";
	end signal_layer_invalid;





	
	
	
	function to_layers (
		line			: in type_fields_of_line; -- layers 1 3 17
		check_layers	: in et_pcb_stack.type_layer_check)
		return pac_signal_layers.set 
	is
		
		use pac_signal_layers;
		layers 		: pac_signal_layers.set; -- to be returned
		cursor 		: pac_signal_layers.cursor;
		inserted	: boolean;
		layer 		: type_signal_layer;
		place 		: type_field_count_positive := 2; -- we start reading the layer numbers with field 2

		
		function f (
			line		: in type_fields_of_line; 
			position	: in type_field_count_positive) 
			return string renames get_field;
												 
		field_2			: constant string := f (line, 2);
		field_2_first	: constant positive := field_2'first;
		field_2_last	: constant positive := field_2'last;

		
		procedure validate_layer (c : in pac_signal_layers.cursor) is begin
			if not signal_layer_valid (element (c), check_layers) then
				signal_layer_invalid (line, element (c), check_layers);
			end if;
		end validate_layer;
	
		
	begin -- to_layers
		
		-- Test the first character of the 2nd field.
		-- If it is the start mark of a layer term like [1, 3, 6-11]
		-- then it must be converted to a set of layers.
		-- Otherwise we assume the layer numbers are given in a
		-- row of discrete layer ids like "1 4 10"
		if field_2 (field_2_first) = layer_term_start then

			layers := to_layers (field_2);

			-- Iterate layers and validate each of them.
			layers.iterate (validate_layer'access);
			
		else -- discrete layer ids like "1 4 10"
			while place <= get_field_count (line) loop

				-- get the layer number from current place
				layer := to_signal_layer (f (line, place));

				-- Issue warning if signal layer is invalid:
				if not signal_layer_valid (layer, check_layers) then
					signal_layer_invalid (line, layer, check_layers);
				end if;

				-- insert the layer number in the container "layers"
				insert (
					container	=> layers,
					new_item	=> layer,
					inserted	=> inserted,
					position	=> cursor);

				-- warn if layer already in container
				if not inserted then
					
					--log (WARNING, affected_line (line) & "signal layer " & to_string (layer) 
					--& " specified multiple times !");
					
					-- CS raise semantic_error_1 with
					raise constraint_error with
						"ERROR: " & get_affected_line (line) 
						& "Signal layer " & to_string (layer) 
						& " specified multiple times !";
				end if;
				
				place := place + 1; -- advance to next place
			end loop;
		
		end if;
		
		return layers;
	end to_layers;

	
	
end et_pcb_stack;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
