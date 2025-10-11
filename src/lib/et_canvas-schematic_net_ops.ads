------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                     CANVAS SCHEMATIC NET OPERATIONS                      --
--                                                                          --
--                               S p e c                                    --
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

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--




generic

	
package et_canvas.schematic_net_ops is





-- RENAME WINDOW:

	-- The window to rename nets is used in all domains
	-- and in various situations. For this reason we make it
	-- a generic object:
	rename_window : gtk.window.gtk_window;

	-- This flag indicates that the rename window is open:
	rename_window_open : boolean := false;
	
	-- This is the field inside the rename_window 
	-- where the operator enters the new name of a net:
	rename_new : gtk_gentry;
	
	-- This is the field inside the rename_window 
	-- where the old name of a net is shown:
	rename_old : gtk_gentry;

	
	-- This procedure assembles the rename_window with 
	-- all its basic properties. It sets the title of the 
	-- window with the targeted net name so that the operator
	-- knows what net it is about.
	-- It connects the "on_key_press_event" with the
	-- callback function cb_rename_window_key_pressed (see below).
	-- This procedure DOES NOT show the rename window. It just prepares
	-- basic things. The actual showing is preformed by a procedure in
	-- the package where the canvas is instantiated (see
	-- for example procedure show_rename_window 
	-- in et_canvas_schematic_nets):
	procedure build_rename_window;



	
	
private
	
	

-- RENAME WINDOW:

	-- See comments on rename window above.
	
	-- This callback function is called whenever
	-- the operator presses a key in the rename window.
	-- If ESC key pressed, then the window is destroyed
	-- by emitting the "destroy" signal. The connection
	-- to the "destroy" signal is estabilshed in the package
	-- where the canvas is instantiated.
	-- All other key-press events are propagated to the
	-- field where the new name is entered (see 
	-- variable "rename_new" above):
	function cb_rename_window_key_pressed (
		window	: access gtk_widget_record'class;
		event	: gdk_event_key)
		return boolean;
	

	access_cb_rename_window_key_pressed : constant
		cb_gtk_widget_gdk_event_key_boolean := cb_rename_window_key_pressed'access;



	
	
end et_canvas.schematic_net_ops;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
