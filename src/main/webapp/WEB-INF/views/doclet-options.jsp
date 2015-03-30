<%@taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c" %>

<div id='look_and_feel'>

<h4>Look and Feel</h4>

<div class='codebox'>
  <table style='margin-bottom: 10px;'>

    <tr>
      <td class='key'>Label annotation</td>
      <td>
        <form method='POST' action='.'>
        <input type='hidden' name='property' value='optionLabelUri' />
        <input type='text' name='value' value='${options.optionLabelUri}' />
        <input type='submit' value='ok' /></form>
      </td>
    </tr>

    <tr>
      <td class='key'>Label language</td>
      <td>
        <form method='POST' action='.'>
          <input type='hidden' name='property' value='optionLabelLang' />
          <input type='text' name='value' value='${options.optionLabelLang}' />
          <input type='submit' value='ok' />
        </form>
      </td>
    </tr>

    <tr>
      <td class='key'>Label property</td>
      <td>
      <form method='POST' action='.'>
        <input type='hidden' name='property' value='optionLabelPropertyUri' />
        <input type='text' name='value' value='${options.optionLabelPropertyUri}' />
        <input type='submit' value='ok' /></form>
      </td>
    </tr>

    <tr>
    <td class='key'>CSS file</td>
    <td>
    <form method='POST' action='.'>
    <input type='hidden' name='property' value='optionDefaultCSS' />
    <input type='text' name='value' value='${options.optionDefaultCSS}' /><input type='submit' value='ok' /></form>
    </td></tr>
    <tr>
    <td class='key'>Show hierarchies</td>
    <td>

    <form>
    <input type='hidden' name='property' value='optionShowMiniHierarchies' />
    <input type='checkbox' class="option" name="value"
      <c:if test="${options.optionShowMiniHierarchies == 'true'}">
        checked
      </c:if>
    />
    </form>
    </td></tr>

    <tr>
    <td class='key'>Render permalinks</td>
    <td>
    <form>
    <input type='hidden' name='property' value='optionRenderPermalink' />
    <input type='checkbox' class="option" name="value"
      <c:if test="${options.optionRenderPermalink == 'true'}">
        checked
      </c:if>
    />
    </form>
    </td></tr>

  </table>
</div>
</div><!-- look and feel -->


<div id='model'>
  <h4>Reasoner</h4>
  <div class='codebox'>
    <table style='margin-bottom: 10px;'>

      <tr>
        <td class='key'>Reasoner</td>
        <td>
          <form>
            <input type='hidden' name='property' value='optionReasoner' />
            <select class="option" name="value">
              <c:forEach items="${reasoners}" var="reasoner">
                <option value='${reasoner}'
                  <c:if test="${reasoner == options.optionReasoner}">
                  selected='selected'
                  </c:if>
                  >${reasoner}</option>
              </c:forEach>
            </select>
          </form>
        </td>
      </tr>

      <tr>
      <td class='key'>Reasoner URL</td>
      <td>
      <form method='POST' action='.'>
      <input type='hidden' name='property' value='optionRemote' />
      <input type='text' name='value' value='${options.optionRemote}' />
      <input type='submit' value='ok' /></form>
      </td></tr>

      <tr>
      <td class='key'>Reasoner enabled</td>
      <td>

      <form>
      <input type='hidden' name='property' value='optionReasonerEnabled' />
      <input type='checkbox' class="option" name="value"
        <c:if test="${options.optionReasonerEnabled == 'true'}">
          checked
        </c:if>
      />
      </form>
      </td></tr>

      <tr>
      <td class='key'>Show inferences</td>
      <td>
      <form>
      <input type='hidden' name='property' value='optionShowInferences' />
      <input type='checkbox' class="option" name="value"
        <c:if test="${options.optionShowInferences == 'true'}">
          checked
        </c:if>
      />
      </form>
      </td></tr>

    </table>
  </div>
</div><!-- model -->