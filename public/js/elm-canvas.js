if (window["customElements"]) {
  customElements.define(
    "elm-canvas",
    class extends HTMLElement {
      constructor() {
        super();
        this.commands = [];
        this.mounted = false;
      }

      set cmds(values) {


        if (this.commands.length !== values.length){

          this.commands = values
          this.render()
          // console.log('cmds length')

        } else if (
          values.some((cmd,i)=>{
            const _cmd = this.commands[i]
            if (cmd.type!==_cmd.type) {
              // console.log('cmds type',cmd.type,_cmd.type)
              return true
            }
            if (cmd.name!==_cmd.name) {
              // console.log('cmds name',cmd.name,_cmd.name)
              return true
            }
            if (cmd.type==='function'){
              if (cmd.args.length!==_cmd.args.length) {
                // console.log('cmds cmd length',cmd.args,_cmd.args)
                return true
              }
              for (let a = 0, l = cmd.args.length; a<l;a++)
                if (cmd.args[a]!==_cmd.args[a]) {
                  // console.log('cmds cmd arg',cmd.args[a],_cmd.args[a])
                  return true
                }
            } else if (cmd.type==='field') {
              if (cmd.value!==_cmd.value) {
                // console.log('cmds cmd value',cmd.value,_cmd.value)
                return true
              }
            }
          })
        ) {

          this.commands = values
          this.render()
          // console.log('cmds')

        }

        // this.commands = values
        // this.render()
        // console.log('cmds')
      }

      static get observedAttributes() {
        return ["width", "height"];
      }

      connectedCallback() {
        // Wait for the inner elements to be rendered before using them
        requestAnimationFrame(() => {
          this.canvas = this.querySelector("canvas");
          this.context = this.canvas.getContext("2d");
          this.mounted = true;

          this.setCanvasDimensions();

          this.render();
        });
      }

      attributeChangedCallback(name, oldValue, newValue) {
        if ((name === "width" || name === "height") && oldValue !== newValue) {
          // Wait for Elm to finish rendering and setting its stuff before
          // changing the inner canvas dimensions
          requestAnimationFrame(() => {
            this.setCanvasDimensions();
          });
        }
      }

      setCanvasDimensions() {
        if (!this.mounted) return;

        // Get dimensions from the elm-canvas element. If they are not set, try to
        // get them from the canvas element inside (to support elm-canvas@3.0.3)
        var width = Number(
          this.getAttribute("width") || this.canvas.getAttribute("width")
        );
        var height = Number(
          this.getAttribute("height") || this.canvas.getAttribute("height")
        );

        var devicePixelRatio = window.devicePixelRatio || 1;
        this.canvas.style.width = width + "px";
        this.canvas.style.height = height + "px";
        this.canvas.width = width * devicePixelRatio;
        this.canvas.height = height * devicePixelRatio;
        // Reset current transformation matrix to the identity matrix
        this.context.setTransform(1, 0, 0, 1, 0, 0);
        this.context.scale(devicePixelRatio, devicePixelRatio);
      }

      render() {
        if (!this.mounted) return;
        // Iterate over the commands in reverse order as that's how the Elm side
        // builds them with linked lists
        for (let i = this.commands.length - 1; i >= 0; i--) {
          this.execCommand(this.commands[i]);
        }
        // this.commands = [];
      }

      execCommand(cmd) {
        if (cmd.type === "function") {
          this.context[cmd.name](...cmd.args);
        } else if (cmd.type === "field") {
          this.context[cmd.name] = cmd.value;
        }
      }
    }
  );
} else {
  throw new Error(
    "window.customElements does not exist. Please use an appropriate polyfill"
  );
}
