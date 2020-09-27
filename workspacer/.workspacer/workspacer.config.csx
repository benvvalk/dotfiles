#r "C:\Program Files\workspacer\workspacer.Shared.dll"
#r "C:\Program Files\workspacer\plugins\workspacer.Bar\workspacer.Bar.dll"
#r "C:\Program Files\workspacer\plugins\workspacer.ActionMenu\workspacer.ActionMenu.dll"
#r "C:\Program Files\workspacer\plugins\workspacer.FocusIndicator\workspacer.FocusIndicator.dll"

using System;
using workspacer;
using workspacer.Bar;
using workspacer.ActionMenu;
using workspacer.FocusIndicator;

/// <summary>
/// Build an interactive menu listing the titles of all
/// windows that are currently managed by Workspacer.
/// </summary>
ActionMenuItemBuilder BuildWindowMenu(ActionMenuPlugin actionMenu, IConfigContext context)
{
    var builder = actionMenu.Create();
    var workspaces = context.WorkspaceContainer.GetAllWorkspaces();
    foreach (var workspace in workspaces)
    {
        foreach (var window in workspace.ManagedWindows)
        {
            var text = $"[{workspace.Name}] {window.Title}";
            builder.Add(text, () => context.Workspaces.SwitchToWindow(window));
        }
    }
    return builder;
}

/// <summary>
/// The method that performs Workspacer configuration.
/// </summary>
Action<IConfigContext> doConfig = (context) =>
{
    context.AddBar();
    context.AddFocusIndicator();
    var actionMenu = context.AddActionMenu();

    var mod = KeyModifiers.Control | KeyModifiers.Alt;

    context.Keybinds.UnsubscribeAll();
    
    context.Keybinds.Subscribe(mod, Keys.J,
        () => context.Workspaces.FocusedWorkspace.FocusNextWindow(), "focus next window");

    context.Keybinds.Subscribe(mod, Keys.K,
        () => context.Workspaces.FocusedWorkspace.FocusPreviousWindow(), "focus previous window");

    context.Keybinds.Subscribe(mod, Keys.Space,
        () => context.Workspaces.FocusedWorkspace.NextLayoutEngine(), "next layout");

    context.Keybinds.Subscribe(mod, Keys.B,
        () => actionMenu.ShowMenu(BuildWindowMenu(actionMenu, context)), "switch window");

    context.Keybinds.Subscribe(mod | KeyModifiers.Shift, Keys.J,
        () => context.Workspaces.FocusedWorkspace.SwapFocusAndNextWindow(), "swap with next window");

    context.Keybinds.Subscribe(mod | KeyModifiers.Shift, Keys.K,
        () => context.Workspaces.FocusedWorkspace.SwapFocusAndPreviousWindow(), "swap with previous window");

    context.Keybinds.Subscribe(mod, Keys.H,
        () => context.Workspaces.FocusedWorkspace.ShrinkPrimaryArea(), "shrink primary area");

    context.Keybinds.Subscribe(mod, Keys.L,
        () => context.Workspaces.FocusedWorkspace.ExpandPrimaryArea(), "expand primary area");

    context.Keybinds.Subscribe(mod, Keys.Oemcomma,
        () => context.Workspaces.FocusedWorkspace.IncrementNumberOfPrimaryWindows(), "increment # primary windows");

    context.Keybinds.Subscribe(mod, Keys.OemPeriod,
        () => context.Workspaces.FocusedWorkspace.DecrementNumberOfPrimaryWindows(), "decrement # primary windows");

    context.Keybinds.Subscribe(mod | KeyModifiers.Shift, Keys.Q, context.Quit, "quit workspacer");

    context.Keybinds.Subscribe(mod, Keys.Q, context.Restart, "restart workspacer");

    context.Keybinds.Subscribe(mod, Keys.D1,
        () => context.Workspaces.SwitchToWorkspace(0), "switch to workspace 1");

    context.Keybinds.Subscribe(mod, Keys.D2,
        () => context.Workspaces.SwitchToWorkspace(1), "switch to workspace 2");

    context.Keybinds.Subscribe(mod, Keys.D3,
        () => context.Workspaces.SwitchToWorkspace(2), "switch to workspace 3");

    context.Keybinds.Subscribe(mod, Keys.D4,
        () => context.Workspaces.SwitchToWorkspace(3), "switch to workspace 4");

    context.Keybinds.Subscribe(mod, Keys.D5,
        () => context.Workspaces.SwitchToWorkspace(4), "switch to workspace 5");

    context.Keybinds.Subscribe(mod, Keys.D6,
        () => context.Workspaces.SwitchToWorkspace(5), "switch to workspace 6");

    context.Keybinds.Subscribe(mod, Keys.D7,
        () => context.Workspaces.SwitchToWorkspace(6), "switch to workspace 7");

    context.Keybinds.Subscribe(mod, Keys.D8,
        () => context.Workspaces.SwitchToWorkspace(7), "switch to workspace 8");

    context.Keybinds.Subscribe(mod, Keys.D9,
        () => context.Workspaces.SwitchToWorkspace(8), "switch to workspace 9");

    context.Keybinds.Subscribe(mod | KeyModifiers.Shift, Keys.D1,
        () => context.Workspaces.MoveFocusedWindowToWorkspace(0), "switch focused window to workspace 1");

    context.Keybinds.Subscribe(mod | KeyModifiers.Shift, Keys.D2,
        () => context.Workspaces.MoveFocusedWindowToWorkspace(1), "switch focused window to workspace 2");

    context.Keybinds.Subscribe(mod | KeyModifiers.Shift, Keys.D3,
        () => context.Workspaces.MoveFocusedWindowToWorkspace(2), "switch focused window to workspace 3");

    context.Keybinds.Subscribe(mod | KeyModifiers.Shift, Keys.D4,
        () => context.Workspaces.MoveFocusedWindowToWorkspace(3), "switch focused window to workspace 4");

    context.Keybinds.Subscribe(mod | KeyModifiers.Shift, Keys.D5,
        () => context.Workspaces.MoveFocusedWindowToWorkspace(4), "switch focused window to workspace 5");

    context.Keybinds.Subscribe(mod | KeyModifiers.Shift, Keys.D6,
        () => context.Workspaces.MoveFocusedWindowToWorkspace(5), "switch focused window to workspace 6");

    context.Keybinds.Subscribe(mod | KeyModifiers.Shift, Keys.D7,
        () => context.Workspaces.MoveFocusedWindowToWorkspace(6), "switch focused window to workspace 7");

    context.Keybinds.Subscribe(mod | KeyModifiers.Shift, Keys.D8,
        () => context.Workspaces.MoveFocusedWindowToWorkspace(7), "switch focused window to workspace 8");

    context.Keybinds.Subscribe(mod | KeyModifiers.Shift, Keys.D9,
        () => context.Workspaces.MoveFocusedWindowToWorkspace(8), "switch focused window to workspace 9");

    context.Keybinds.Subscribe(mod, Keys.W,
        () => context.Workspaces.SwitchFocusedMonitor(0), "focus monitor 1");

    context.Keybinds.Subscribe(mod, Keys.E,
        () => context.Workspaces.SwitchFocusedMonitor(1), "focus monitor 2");

    context.Keybinds.Subscribe(mod | KeyModifiers.Shift, Keys.W,
        () => context.Workspaces.MoveFocusedWindowToMonitor(0), "move focused window to monitor 1");

    context.Keybinds.Subscribe(mod | KeyModifiers.Shift, Keys.E,
        () => context.Workspaces.MoveFocusedWindowToMonitor(1), "move focused window to monitor 2");

    context.WorkspaceContainer.CreateWorkspaces("1", "2", "3", "4", "5", "6", "7", "8", "9");
};
return doConfig;