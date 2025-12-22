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

with et_string_processing;		use et_string_processing;


package body et_pcb_stack is



	function layer_stacks_equally (
		right, left : in type_signal_layers.set)
		return boolean
	is
		use type_signal_layers;
	begin
		if right = left then
			return true;
		else
			return false;
		end if;
	end layer_stacks_equally;


	

	function layer_stack_contains (
		stack		: type_signal_layers.set;
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


	
	
end et_pcb_stack;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
