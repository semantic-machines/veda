export const html = `
  <table class="table table-bordered">
    <thead class="result-header">
      <tr>
        <th colspan="8" about="v-s:Email" property="rdfs:label"></th>
      </tr>
      <tr class="active">
        <th width="1%"><span class="glyphicon glyphicon-search"></span></th>
        <th width="10%"><span about="v-wf:to" property="rdfs:label"></span></th>
        <th width="10%"><span about="v-wf:from" property="rdfs:label"></span></th>
        <th width="10%" class="orderby" data-orderby="v-s:created"><span about="v-s:created" property="rdfs:label"></span></th>
        <th><span about="v-s:subject" property="rdfs:label"></span></th>
        <th><span about="v-s:origin" property="rdfs:label"></span></th>
      </tr>
    </thead>

    <tbody class="result-container">
      <tr>
        <td><a href="#/@" class="glyphicon glyphicon-search"></a></td>
        <td rel="v-wf:to" about="@" data-template="v-ui:LabelTemplate" ></td>
        <td rel="v-wf:from"  about="@" data-template="v-ui:LabelTemplate"></td>
        <td property="v-s:created"></td>
        <td property="v-s:subject" ></td>
        <td property="v-s:origin" ></td>
      </tr>
    </tbody>
  </table>
`;
